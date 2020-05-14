(ns p6.app
  (:require
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [cljs.reader :refer [read-string]]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]]
   [clojure.string :as str]
   [cljs.pprint :refer [cl-format]]

   [p6.util :as u])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(defonce fs (js/require "fs"))
(defonce electron (js/require "electron"))
(defonce child-process (js/require "child_process"))
(defonce ipcRenderer (.-ipcRenderer electron))
(defonce electron-prompt (js/require "electron-prompt"))
(defn prompt [opts] (electron-prompt (clj->js opts)))

(defonce _initialize-app  ;; Define the application
  (do
    ;; "params" is an atom, so it can be watched
    (def params (r/atom {"t" 0}))
    (def fps 25)
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

    (def keymap
      (r/atom {";" prompt-set-param}))
    (.addEventListener  ;; Keyboard response according to keymap
     js/window "keydown" (fn [kbe]  ;; Keyboard event
                           (let [response (or (@keymap (.-key kbe))
                                              (fn [] (do)))]
                             (response))))

    (defonce draw (atom (fn [params] (do))))

    (do  ;; Controls
      (defn make-slider [param min max step]
        {:param param :type :slider
         :state (r/atom {:min min :max max :step step
                         :autoplay false :cycle 5})})

      (defn make-select [param options]
        {:type :select :param param :options [0 1]})

      (def t-control (make-slider "t" 0 1 0.1))
      (def control-list (r/atom [t-control]))

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
               [:ul {:style {:padding "10px"}}
                (for [ctr @control-list]  ;; Dynamic controls
                  ^{:key (ctr :param)}
                  [:<>
                   (case (ctr :type)
                     :custom [(ctr :component)]
                     :slider [slider (ctr :param) (ctr :state)]
                     :select [select (ctr :param) (ctr :options)])
                   [:hr]])])]))))

    (def mouse-pos (atom [0 0]))  ;; Track mouse position
    (rdom/render  ;; Render the app
     [:<>
      [:canvas#main-canvas
       {:style {:display "block"}
        ;; Update mouse position every time the mouse moves
        :on-mouse-move (fn [e] (reset! mouse-pos
                                       [(.-clientX e) (.-clientY e)]))}]
      [controls]]
     (.getElementById js/document "app"))

    (do   ;; Initialize the canvas
      (def canvas (.getElementById js/document "main-canvas"))
      (def ctx    (.getContext canvas "2d"))
      (def w (.-innerWidth  js/window))
      (def h (.-innerHeight js/window))
      (set! (.-width  canvas) w)
      (set! (.-height canvas) h)
      (do  ;; Color & Style Setup
        ;; We usually don't need any implicit state
        ;; But it's nice to setup color for experimentation
        (set! (.-strokeStyle ctx) "red")
        (set! (.-fillStyle ctx)
              (-> (assoc u/green :a 0.25) u/css))
        (set! (.-font ctx) "normal 20px Arial")))

    (do  ;; Recording business
      (def recording? (r/atom false))  ;; Flag to tell if a recording is going on, so that the animation cycle won't have any effects

      (defn record []
        (reset! recording? true)
        (println "Recording started")

        (let [path "/home/khoa/note/data/recorded/canvas.txt"
              stream (.createWriteStream fs path)
              vid-time (let [t-state @(t-control :state)]
                         (t-state :cycle))
              t-step (-> (/ 1 fps) ((u/to-01 0 vid-time)))]
          (doseq [[t i]  (map vector  ;; A transposition
                              (concat (range 0 1 t-step) [1])
                              (range))]
            (set-param "t" t)
            (let [data
                  ^js/String (-> canvas (.toDataURL) (.split ",") (last))]
              (cond (= t 1) (.write stream data)
                    :else   (.write stream (str data "\n")))))
          ;; Done writing
          (.end stream))

        (println "Converting to video")
        (.exec child-process
               "python3 /home/khoa/note/data/recorded/main.py"
               (fn [error stdout stderr]
                 (println "Conversion to video done (or errored out)!")
                 (println {:error error})))

        (println "Recording is done!")
        ;; Convert data to video, too!
        (reset! recording? false))

      ;; Add recording button to the control list
      (swap! control-list
             (fn [l]
               (cons {:type      :custom
                      :component (fn []
                                   (let [rec? @recording?]
                                     [:button#rec-btn
                                      {:on-click (fn [e] (record))
                                       ;; Disabled when recording
                                       :disabled rec?
                                       :style {:font-size "20px"}}
                                      (cond rec?  "Recording..."
                                            :else "Rec")]))}
                     l))))

    true))

(def the-frame (-> (u/dmat) (.scale w h)))
(def the-square-frame (u/square-frame the-frame))
(do  ;; Zooming & Panning
  (reset! params
          ;; view-frame is initially "the-frame"
          (merge {"view-frame" the-frame} @params))

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
          [x y]   [(- (/ w 2) mx) (- (/ h 2) my)]]
      (update-param "view-frame"
                    (fn [f]
                      (new js/DOMMatrix [(.-a f) (.-b f) (.-c f) (.-d f)
                                         (+ (.-e f) x) (+ (.-f f) y)])))))

  (reset! keymap (merge @keymap {"i" #(zoom (* w 0.25))
                                 "o" #(zoom (* w -0.25))
                                 "m" teleport
                                 "0" #(set-param "view-frame" the-frame)})))

(do  ;; Primitive Painters
  (defn text-ratio
    "Get width:height ratio of the text (assuming it scales uniformly)"
    [text]
    (u/lift-ctx
     ctx
     (fn [] (set! (.-font ctx) (str "Normal 1px Arial")))
     (fn [] (let [m (.measureText ctx text)]
              (.-width m)))))

  (defn painter
    "Paint a shape"
    [{:keys [path text fill stroke line-width]}]
    {:pre [(or path text)]}
    (fn [frame]
      (cond path  ;; Draw path
            (do (u/lift-ctx
                 ctx
                 (fn [] (.setTransform ctx frame))
                 (fn []
                   (.beginPath ctx)
                   (doseq [[op a0 a1 a2 a3 a4 a5] path]
                     (case op
                       :move-to    (.moveTo ctx a0 a1)
                       :line-to    (.lineTo ctx a0 a1)
                       :arc        (.arc ctx a0 a1 a2 a3 a4 a5)
                       :close-path (.closePath ctx)))))
                ;; Stroke outside of transformation
                (u/lift-ctx
                 ctx
                 (fn []
                   (set! (.-fillStyle ctx)   (u/css fill))
                   (set! (.-strokeStyle ctx) (u/css stroke))
                   (set! (.-lineWidth ctx) line-width))
                 (fn [] (.stroke ctx) (.fill ctx))))

            text   ;; Draw text that fits nicely into the frame (no stretching)
            (let [frame (-> frame (u/force-ratio (text-ratio text)))]
              (u/lift-ctx
               ctx
               (fn []
                 ;; We draw at [0 0], so the text is below that
                 (set! (.-textBaseline ctx) "top")
                 ;; (font height) = (frame height)
                 (set! (.-font ctx)
                       (str "Normal " (u/frame-height frame) "px" " Arial"))
                 (set! (.-fillStyle ctx)   (u/css fill))
                 (set! (.-strokeStyle ctx) (u/css stroke))
                 (set! (.-lineWidth ctx) line-width))
               (fn []
                 (let [[x y] (u/transform frame [0 0])]
                   (.fillText   ctx text x y)
                   (.strokeText ctx text x y))))))))

  (defn textp [& args]
    (painter (apply u/text args)))

  (defn linep [& args]
    (painter (apply u/line args)))

  (defn circp [& args]
    (painter (apply u/circ args)))

  (defn rectp [& args]
    (painter (apply u/rect args)))

  (defn dot
    "Painter of a rectangle that resembles a dot.
    Note: This is a non-relative painter."
    ([x y] (dot x y {}))
    ([x y style]
     (fn [frame]
       (let [[X Y] (u/transform frame [x y])]
         (u/lift-ctx
          ctx
          (fn [] (set! (.-fill ctx) (u/css style)))
          #(.fillRect ctx X Y 3 3))))))

  (defn label
    "Painter of a label"
    ([txt x y] (label txt x y nil))
    ([txt x y color]
     (fn [frame]
       (let [[X Y] (u/transform frame [x y])
             metrics     (.measureText ctx txt)
             text-width  (.-width metrics)
             text-height (- (.-actualBoundingBoxAscent  metrics)
                            (.-actualBoundingBoxDescent metrics))
             ;; Constrain the text so it does not go out of bound
             [Xc,Yc] [(min X (- w text-width))
                      (max Y text-height)]]
         (u/lift-ctx
          ctx
          (fn []
            ;; view it as a stroke
            (set! (.-fillStyle ctx) (u/css (or color u/default-stroke)))
            ;; cast a black aura around it
            (set! (.-strokeStyle ctx) (u/css (u/hsl 0 0 0))))
          #(.fillText ctx txt Xc Yc))))))

  (defn draw-grid
    "Painter of a grid, in a given unit frame"
    ([arg] (draw-grid arg {}))
    ([{:keys [xmin ymin xmax ymax]}
      {:keys [step style]
       :or {step 1 style (u/hsl 0 0 50)}}]
     (fn [frame]
       (doseq [x (concat (range xmin xmax step) [xmax])]
         ((label x x 0) frame)
         ((painter (u/line [x ymin] [x ymax] {:stroke style}))
          frame))
       (doseq [y (concat (range ymin ymax step) [ymax])]
         ((label y 0 y) frame)
         ((painter (u/line [xmin y] [xmax y] {:stroke style}))
          frame))))))

(defn render-conf
  "Just draw all the objects in `conf`
   Note: objects and their identities don't matter in rendering."
  [conf]
  (doseq [{:keys [shapes frame]} (vals conf)]
    (doseq [shape shapes]
      ((painter shape) frame))))

(defn render [] (reset! params @params))

(defn set-draw! [fun]
  (reset! draw fun)
  (render))

(do ;; Invoke the framework!
  ;; Define these first:
  ;; "params" (a map of parameters)
  ;; "draw" (a no-argument function)
  (add-watch params :draw  ;; Redraw each time "params" change
             (fn [_ _ _ _]
               (cond (false? (@params "clr")) ()
                     :else (-> ctx (.clearRect 0 0 w h)))
               (@draw @params)))

  ;; Update cycle to adjust parameters (simulate animation)
  (let [delay (/ 1000 fps)]
    (defn time-loop [time]
      (when (not @recording?)  ;; Don't play anything in recording mode
        (doseq [ctr @control-list
                :when (and (ctr :state)
                           (@(ctr :state) :autoplay))]
          ;; Loop over the controls that are playing
          (let [param                   (ctr :param)
                {:keys [min max cycle]} @(ctr :state)]
            (let [inc (-> delay
                          ((u/convert-interval 0 (* 1000 cycle)  ;; cycle in sec
                                               min max)))]
              (update-param param
                            #(mod (+ % inc) max))))))

      (go  ;; Request next frame
        (<! (timeout delay))
        (.requestAnimationFrame js/window time-loop))))

  (defonce _begin-time-loop
    (do
      (.requestAnimationFrame js/window time-loop)
      true))

  (render))
