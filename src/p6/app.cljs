(ns p6.app
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   ["fs" :as fs]
   ["os" :as os]
   ["electron" :as electron]
   ["electron-prompt" :as electron-prompt]
   ["child_process" :as child-process]

   [cljs.reader :refer [read-string]]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]]
   [clojure.string :as str]
   [cljs.pprint :refer [cl-format]]

   [p6.util :as u])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(defn prompt [opts] (electron-prompt (clj->js opts)))

(defonce params (r/atom {"t" 0}))
(def fps 15)
(defn set-param [param val]
  (swap! params assoc param val))
(defn update-param [param updater]
  (swap! params update param updater))
(defn prompt-set-param []
  (-> (prompt {:label "Enter parameter name"})
      (.then
       (fn [param]
         (cond
           (empty? param) (do)
           (not (@params param)) (throw "Parameter doesn't exist")
           :else (-> (prompt {:label (str "Enter new value. Current: "
                                          (@params param))})
                     (.then (fn [v] (let [v (read-string v)]
                                      (set-param param v))))))))))
(defonce keymap
  (r/atom {";" prompt-set-param}))

(defonce _key_listener
  (boolean
   (.addEventListener  ;; Keyboard response according to keymap
    js/window "keydown" (fn [kbe]  ;; Keyboard event
                          (let [response (or (@keymap (.-key kbe))
                                             (fn [] (do)))]
                            (response))))))

(defonce draw (atom (fn [params] (do))))

(defonce mouse-pos (atom [0 0]))  ;; Track mouse position

(defn make-slider [param min max step]
  {:param param :type :slider
   :state (r/atom {:min min :max max :step step
                   :autoplay false :cycle 5})})

(defonce t-control (make-slider "t" 0 1 0.1))

(declare canvas)  ;; The recording depends on canvas, which doesn't exist until the entire UI is defined

(do  ;; Recording business
  (defonce recording?  ;; Flag to tell if a recording is going on, so that the animation cycle won't have any effects
    (r/atom false))

  (defn record []
    (reset! recording? true)
    (println "Recording started")

    (let [capturer (js/CCapture.
                    (clj->js {:format "webm",
                              :quality 0.0,
                              :framerate fps}))
          vid-time (let [t-state @(t-control :state)]
                     (t-state :cycle))
          t-step (-> (/ 1 fps) ((u/to-01 0 vid-time)))]
      (.start capturer)
      (doseq [[t i]  (map vector  ;; A transposition
                          (concat (range 0 1 t-step) [1])
                          (range))]
        (set-param "t" t)
        (.capture capturer canvas))

      ;; Done writing
      (println "Recording is done. Showing the video...")
      (.stop capturer)
      (.save capturer)
      (println "File written!")
      (reset! recording? false)))

  (defn record-btn []
    [:button#rec-btn
     {:on-click (fn [e] (record))
      :disabled @recording?  ;; Disabled when recording
      :style {:font-size "20px"}}
     (cond @recording? "Recording..."
           :else "Rec")]))

(do  ;; Capture
  (defn capture []
    (println "Capturing canvas")
    (.toBlob canvas
             (fn [blob]
               (js/download blob "canvas.png" "image/png"))))

  (defn capture-btn []
    [:button#rec-btn
     {:on-click (fn [e] (capture))
      :style {:font-size "20px"}}
     "Capture"]))

(defn make-select [param options]
  {:type :select :param param :options [0 1]})

(defonce control-list (r/atom [t-control]))

(defn select [param options]
  [:div
   param " = "
   [:select {:value (@params param)
             :on-change (fn [e]
                          (set-param param
                                     (-> e (.-target) (.-value) (read-string))))
             :style {:font-size "inherit"}}
    (for [option options]
      [:option {:value option} (str option)])]])

(defn slider [param state]
  (let [{:keys [min max step autoplay cycle]} @state
        in-btn (fn [key]
                 [:button {:style {:font-size "inherit" :padding "0"}
                           :on-click
                           (fn []
                             (-> (prompt {:label "Enter param value"})
                                 (.then
                                  (fn [v]
                                    (let [v (js/parseFloat v)]
                                      (when-not (js/isNaN v)
                                        (swap! state assoc key v)))))))}
                  (@state key)])]
    (fn [param state]
      [:<>
       [:input  ;; The range input
        {:type "range" :value (@params param)
         :min (@state :min) :max (@state :max) :step (@state :step)
         :on-change #(let [v (-> % .-target .-value (js/parseFloat))]
                       (set-param param v))}]
       [:div {:style {:display "inline"}} ;; Slider Steps
        " Step: " (in-btn :step)]

       [:div {:style {:display "block"}}  ;; Value and range
        (in-btn :min) " ≤ "
        param " = " (-> (@params param)
                        (u/round 0.05)
                        (->> (cl-format nil "~,2F")))
        " ≤ " (in-btn :max)]

       [:div {:style {:display "block"}}  ;; autoplay
        "Play " [:input {:type "checkbox" :style {:u/transform "scale(2)"}
                         :checked (@state :autoplay)
                         :on-change (fn [e]
                                      (swap! state update :autoplay not))}]
        " in " (in-btn :cycle) " sec"]])))

(defn controls []
  ;; A collapsible side-panel displaying all controls
  (let [show? (r/atom "false")]
    (fn []
      [:div#controls {:style {:position "fixed"
                              :z-index "1"
                              :top "0" :left "0"
                              :background "rgba(0,0,0,0.8)"}}
       [:button#menu-btn  ;; Button to toggle menu
        {:on-click (fn [e] (swap! show? not))
         :style {:font-size "20px"}}
        "☰"]

       (when @show?
         [:div {:style {:padding "10px"}}
          [record-btn]
          [capture-btn]
          [:hr]
          (for [ctr @control-list]  ;; Dynamic controls
            [:<>
             {:key (or (:param ctr)
                       (;; Some controls aren't tied to a parameter
                        :key ctr))}
             (case (ctr :type)
               :custom [(ctr :component)]
               :slider [slider (ctr :param) (ctr :state)]
               :select [select (ctr :param) (ctr :options)])
             [:hr]])])])))

(rdom/render  ;; Render the app
 [:<>
  [:canvas#main-canvas
   {:style {:display "block"}
    ;; Update mouse position every time the mouse moves
    :on-mouse-move (fn [e] (reset! mouse-pos
                                   [(.-clientX e) (.-clientY e)]))}]
  [controls]]
 (.getElementById js/document "app"))

(defonce canvas (.getElementById js/document "main-canvas"))
(defonce ctx    (.getContext canvas "2d"))
(defonce w (atom (.-innerWidth  js/window)))
(defonce h (atom (.-innerHeight js/window)))
(defn render []
  (cond (false? (@params "clr")) ()
        :else (-> ctx (.clearRect 0 0 @w @h)))
  (@draw @params))

(defn get-the-frame [] (.scale (u/dmat) @w @h))
(defonce the-frame (atom (get-the-frame)))
(defn get-square-frame [] (u/force-square @the-frame))
(defonce square-frame (atom (get-square-frame)))

(defonce  _init_canvas
  (boolean
   (do
     (set! (.-width  canvas) @w)
     (set! (.-height canvas) @h)
     (.addEventListener  ;; Rerender each time the window size changes
      js/window "resize"
      (fn [e]
        (reset! w (.-innerWidth  js/window))
        (reset! h (.-innerHeight js/window))
        (set! (.-width  canvas) @w)
        (set! (.-height canvas) @h)
        (reset! the-frame (get-the-frame))
        (reset! square-frame (get-square-frame))
        (render)))
     (do  ;; Color & Style Setup
       ;; We usually don't need any implicit state
       ;; But it's nice to setup color for experimentation
       (set! (.-strokeStyle ctx) "red")
       (set! (.-fillStyle ctx)
             (-> (assoc u/green :a 0.25) u/css))
       (set! (.-font ctx) "normal 20px Arial")))))

(do  ;; Zooming & Panning
  (defonce _add-view-frame
    (boolean
     (reset! params
             ;; view-frame is initially "the-frame"
             (merge {"view-frame" @the-frame} @params))))

  (defn zoom [inc]
    (update-param "view-frame"
                  (fn [F]
                    (let [[mx my] @mouse-pos
                          a (.-a F), d (.-d F), e (.-e F), f (.-f F)
                          ;; The coordinate of the mouse, according to F
                          [mxF myF] [(/ (- mx e) a)
                                     (/ (- my f) d)]
                          ;; Updating process
                          a+ (+ a inc) d+ (+ d inc)
                          ;; Afterwards, the mouse's coor will be rendered at
                          ;; [(mxF*(a+) + e+) (myF*(b+) + f+)]
                          ;; We need it to be
                          ;; [(mxF*a    + e)  (myF*b    + f)]
                          e+ (- e (* mxF inc))
                          f+ (- f (* myF inc))]
                      (new js/DOMMatrix [a+ (.-b F), (.-c F) d+, e+ f+])))))
  (defn teleport []
    (let [[mx my] @mouse-pos
          ;; [x y] is the vector from screen center → mouse
          [x y]   [(- (/ @w 2) mx) (- (/ @h 2) my)]]
      (update-param "view-frame"
                    (fn [f]
                      (new js/DOMMatrix [(.-a f) (.-b f) (.-c f) (.-d f)
                                         (+ (.-e f) x) (+ (.-f f) y)])))))

  (defonce _add-view-frame-movement
    (boolean
     (swap! keymap
            merge {"i" #(zoom (* @w 0.25))
                   "o" #(zoom (* @w -0.25))
                   "m" teleport
                   "0" #(set-param "view-frame" the-frame)}))))

(defn set-draw! [fun]
  (reset! draw fun)
  (render))

(add-watch params :draw  ;; Redraw each time "params" change
           (fn [_ _ _ _] (render)))

(defn time-loop
  "Update cycle to adjust automated controls"
  [time]
  (let [delay (/ 1000 fps)]
    (when-not @recording?  ;; Don't adjust controls in recording mode
      (doseq [ctr @control-list
              :when (and (ctr :state)
                         (@(ctr :state) :autoplay))]
        ;; Loop over the controls that are playing
        (let [param (ctr :param)
              {:keys [min max cycle]} @(ctr :state)]
          (let [inc (-> delay
                        ((u/convert-interval 0 (* 1000 cycle)  ;; cycle in sec
                                             min max)))]
            (update-param param
                          #(mod (+ % inc) max))))))

    (go  ;; Request next frame
      (when-not @recording?  ;; No need to wait in recording mode
        (<! (timeout delay)))
      (.requestAnimationFrame js/window time-loop))))

(defonce _begin-time-loop
  (boolean (.requestAnimationFrame js/window time-loop)))
(render)
