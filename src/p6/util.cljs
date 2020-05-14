(ns p6.util
  (:require
   [clojure.string :as str]))

(def pi  js/Math.PI)
(def tau (* 2 pi))
(defn cos [x] (js/Math.cos x))
(defn sin [x] (js/Math.sin x))
(defn round
  ([x] (round x 1))  ;; Round to unit column by default
  ([x precision]
   (let [y (+ x (/ precision 2))]
     (- y (mod y precision)))))

(defn prefix?
  ([ls] (fn [lsx] (prefix? ls lsx)))
  ([ls lsx]
   (let [c (count ls) cx (count lsx)]
     (and (<= c cx)
          (reduce
           (fn ([] true) ([b] b) ([a b] (and a b)))
           (for [i (range c)]
             (= (nth ls i) (nth lsx i))))))))

(defn enumerate [ls]
  (map vector ls (range)))

(defn transform [M [x y]]
  (let [p (.transformPoint M (js/DOMPoint. x y))]
    [(.-x p) (.-y p)]))

(defn adjacents [s] (partition 2 1 s))

(defn constrain [n lb ub]
  ;; Too high: keep at "ub"; Too low: keep at "lb"
  (max (min n ub) lb))

(defn convert-interval [a b A B]
  (fn [x]
    ;; The natural correspondence of [a,b] to [A,B]
    ;; x may well be outside of [a,b], the function does what it does
    ;; Note: If (a = b) then (A = B)
    (cond (= a b) (cond (= A B) (+ (- x a) A)  ;; As if d and D cancelled
                        :else (throw "Illegal ranges"))
          (= A B) A  ;; The only collapse
          :else (let [d (- b a), D (- B A)
                      r (/ (- x a) d)]
                  (+ A (* r D))))))

(defn to-01 [a b]
  (convert-interval a b 0 1))
(defn from-01 [A B]
  (convert-interval 0 1 A B))

(do  ;; Easing Functions
  (defn quad-out [x] (+ (- (* x x)) (* 2 x))))

(defmulti css
  (fn [x]
    (cond (string? x) (throw (str "Please don't use raw css string: " x))
          (:h x)      :hsl)))
(defmethod css :hsl [{:keys [h s l a] :or {a 1}}]
  (let [s% (str s "%") l% (str l "%")]
    (str "hsla" "(" (str/join "," [h s% l% a]) ")")))
(defn hsl
  ([h s l]   (hsl h s l 1))
  ([h s l a] {:h h :s s :l l :a a}))
(def transparent (hsl 0 0 0 0))
(def red    (hsl 0 100 50))
(def green  (assoc red :h 33))
(def blue   (assoc red :h 66))
(def white  (assoc red :l 1))
(def grey   (assoc red :s 0 :l 50))
(def gray   grey)
(def black  (assoc red :l 0))
(def yellow (assoc red :h 16))

;; These are defined to initialize color data
(def default-stroke (hsl 0 100 50))
(def default-fill   (hsl 33 100 50 0.2))
(def default-style {:stroke default-stroke
                    :line-width 1
                    :fill default-fill})

(defn polar->cart
  ([theta r]
   [(* (cos theta) r)
    (* (sin theta) r)])
  ([theta] (polar->cart theta 1)))

(defn repoly [n]
  ;; Return the vertices of an n-gon inscribing the frame
  (let [angle (/ tau n)]
    (for [i (range 0 n)]
      (let [[x y] (polar->cart (* i angle) 0.5)]
        [(+ x 0.5) (+ y 0.5)]))))

(defn section-01 [weights]
  ;; Example: [1 2 1] → (0 0.25 0.75 1)
  (let [total (reduce + weights)]
    (reductions (fn [accum w]
                  (+ accum (/ w total)))
                0
                weights)))

(defn rand-item [items weights]
  (let [r (rand)]
    (loop [i 0, [a & d] (-> weights section-01 rest)]
      (cond (< r a) (nth items i)
            :else   (recur (inc i) d)))))

(defn rev-anime [a] (fn [t] (-> t ((from-01 1 0)) a)))

(do  ;; Frames & Frames Transformations
  (defn dmat  ;; DOMMatrix, to work with canvas transformation
    ([] (new js/DOMMatrix))
    ([mat]  ;; Matrix to DOMMatrix: note that "mat" must be 4×4 (3D)
     (new js/DOMMatrix
          (;; The method accepts flattened input
           apply concat mat))))

  (defn pad-frame
    "Scale `frame` down and reposition it.
    `ratio` is the scaling factor"
    [frame ratio]
    (let [tslate (/ (- 1 ratio) 2)]
      (-> frame
          (.translate tslate tslate)
          (.scale ratio ratio))))

  (defn dmat? [x]
    (and (not (nil? x))
         (#{"DOMMatrixReadOnly" "DOMMatrix"}
          (-> x .-constructor .-name))))

  (defn dmat->mat [dm]  ;; Convert a DOMMatrix to a 4×4 matrix
    [[(.-m11 dm) (.-m12 dm) (.-m13 dm) (.-m14 dm)]
     [(.-m21 dm) (.-m22 dm) (.-m23 dm) (.-m24 dm)]
     [(.-m31 dm) (.-m32 dm) (.-m33 dm) (.-m34 dm)]
     [(.-m41 dm) (.-m42 dm) (.-m43 dm) (.-m44 dm)]])



  (defn frame-width  [frame] (.-a frame))
  (defn frame-height [frame] (.-d frame))

  (defn force-ratio [frame ratio]
    ;; "ratio" = w+ : h+
    (let [w (frame-width  frame)
          h (frame-height frame)]
      (let [;; We know one of the dimension will be fixed
            w+ (min w (* h ratio))
            h+ (min h (/ w ratio))
            ws (/ w+ w) hs (/ h+ h)]
        (-> frame
            ;; The vertical gap will be (h - h+), or (1 - hs) in h unit
            ;; Similarly, the horizontal gap will be (1 - ws) in w unit
            (.translate (/ (- 1 ws) 2)
                        (/ (- 1 hs) 2))
            (.scale ws hs)))))

  (defn square-frame
    "Force a frame to be square"
    [frame]
    (force-ratio frame 1))

  (defn remap [bounding-frame {:keys [xmin ymin xmax ymax
                                      rows cols]}]
    ;; returns frame f such that coordinates
    ;; in the given range will be within the "bound-frame"
    (-> bounding-frame
        (.scale (/ 1 (- xmax xmin))
                (/ 1 (- ymax ymin)))
        (.translate (- xmin) (- ymin))))

  (defn dflip [x-or-y dmat]
    (fn [dmat]
      (.multiply dmat
                 (case x-or-y
                   :x (dmat [[-1 0] [0 1] [1 0]])
                   :y (dmat [[1 0]  [0 -1] [0 1]]))))))

(do ;; Shapes
  ;; Definition of a shape
  ;; A shape either contains `path` or `text`
  ;; Its style is specified by `fill` and `stroke`
  (defn line
    "The shape of a line"
    ([[ax ay] [bx by]] (line [ax ay] [bx by] {}))
    ([[ax ay] [bx by] style]
     (merge default-style
            {:path [[:move-to ax ay] [:line-to bx by]]}
            style)))

  (defn text
    "The shape of a string"
    ([txt] (text txt {}))
    ([txt style] (merge {:text txt} default-style style)))

  (defn circ
    "The shape of a circle"
    ([]        (circ 0.5 0.5 0.5 {}))
    ([style]   (circ 0.5 0.5 0.5 style))
    ([cx cy r] (circ cx cy r {}))
    ([cx cy r style]
     (merge default-style
            {:path [[:arc cx cy r 0 tau false]]}
            style
            {:type :circle})))

  (defn segments
    "The shape of segments"
    ([points] (segments points {}))
    ([points style]
     (merge default-style
            {:path (concat [(let [[x y] (first points)]
                              [:move-to x y])]
                           (for [[x y] (rest points)]
                             [:line-to x y]))}
            style)))

  (defn polygon
    "A polygon is any sequence of line segments"
    ([vertices] (polygon vertices {}))
    ([vertices style]
     (-> (segments vertices style)
         (assoc :type :polygon)
         ;; Note: the path must be closed
         (update :path #(concat % [[:close-path]])))))

  (defn rect
    "The shape of a rectangle"
    ([]        (rect 0 0 1 1 {}))
    ([style]   (rect 0 0 1 1 style))
    ([x y w h] (rect x y w h {}))
    ([x y w h style]
     (polygon [[x y] [(+ x w) y] [(+ x w) (+ y h)] [x (+ y h)]] style))))

(defn series
  "Combine anime into one sequence
   The type is [([0,1] → X)] → ([0,1] → X)"
  [& anime-weights]
  (let [anime        (map first anime-weights)
        weights      (map second anime-weights)
        _ (assert (= (count anime) (count weights))
                  (str "Yo your series is illegal!" anime-weights))
        total-weight (reduce + weights)
        begs         (section-01 weights)
        ;; begin-end time of each animation (adjacent pairs of begs)
        intervals    (partition 2 1 begs)]
    (fn [t]
      (loop [[anima & anime-rest]  anime
             [[beg end] & is-rest] intervals]
        ;; If t falls into interval, play the corresponding animation
        ;; Otherwise continue searching
        (cond (and (>= t beg) (<= t end))
              (-> t ((to-01 beg end)) (anima))

              (empty? is-rest) nil
              :else (recur anime-rest is-rest))))))

(do  ;; General Arithmetic Operations
  (defn arith-dispatch [x]
    (cond (nil? x)     nil
          (map? x)     :map
          (fn? x)      :fn
          (number? x)  :num
          (coll? x)    :col
          (dmat? x)  :dmat))
  (defmulti  add (fn [x y] (map arith-dispatch [x y])))
  (defmethod add [:num :num] [x y] (+ x y))
  (defmethod add [:col :col] [x y] (map add x y))
  (letfn  ;; Dealing with maps
      [(f [x y] #(add (x %) (y %)))]
    (defmethod add [:map :map] [x y] (merge-with add x y))
    (defmethod add [:map :fn]  [x y] (f x y))
    (defmethod add [:map :fn]  [x y] (f x y))
    (defmethod add [:map :fn]  [x y] (f x y)))

  (defmethod add [:dmat :dmat] [x y]
    (-> (add (dmat->mat x) (dmat->mat y))
        (dmat)))

  (defmulti  mult (fn [x y] (map arith-dispatch [x y])))
  (defmethod mult [:num :num]  [x y] (* x y))
  (defmethod mult [:num :col]  [x y] (map #(mult x %) y))
  (defmethod mult [:col :col]  [x y] (map mult x y))
  (defn update-map [m f]
    (reduce-kv (fn [m k v] (assoc m k (f v)))
               {} m))
  (defmethod mult [:num :map]  [n m] (update-map m (partial mult n)))
  (defmethod mult [:num :fn]   [n f] (comp (partial mult n) f))
  (defmethod mult [:num :dmat] [n m]
    (->> (dmat->mat m) (mult n) (dmat)))

  (defmulti  neg arith-dispatch)
  (defmethod neg :num  [x] (- x))
  (defmethod neg :col  [c] (map neg c))
  (defmethod neg :map  [x] (update-map x neg))
  (defmethod neg :fn   [x] (comp neg x))
  (defmethod neg :dmat [m] (-> (dmat->mat m) (neg) (dmat)))
  (defmethod neg :default [x]
    (throw (new js/Error (str "neg not supported: " x
                              " of type " (arith-dispatch x)))))
  (defn subtract [x y] (add x (neg y))))

(do  ;; Morphing
  (defmulti  morph (fn [x y] (map arith-dispatch [x y])))
  (defmethod morph [:map :map] [a b]
    (let [m (->> (for [k (keys a)]
                   [k (morph (a k) (b k))])
                 (into {}))]
      (fn [t] (->> (for [[k v] m] [k (v t)])
                   (into {})))))

  (defmethod morph :default [a b]
    (cond (= a b) (constantly a)
          (not a) (constantly b)
          (not b) (constantly a)
          :else   (let [d (subtract b a)]
                    #(case %  ;; arg is Time
                       0 a 1 b
                       (add a (mult % d))))))

  #_(defmethod morph [:polygon :polygon] [p1 p2]
      (let [[vs1 vs2] (map :vertices [p1 p2])
            [vs1 vs2]
            (let [cmax (max (count vs1) (count vs2))
                  f    (fn [vs]
                         (let [c (count vs)]
                           (cond (< c cmax)
                                 (let [q (quot cmax c), r (mod cmax c)]
                                   (mapcat
                                    (fn [v i]
                                      (cond (< i r) (repeat (inc q) v)
                                            :else   (repeat q v)))
                                    vs (range 0 c)))
                                 :else vs)))]
              [(f vs1) (f vs2)])]
        (fn [t]  ;; Returns a polygon
          (let [vs (map (fn [[x1 y1] [x2 y2]]
                          [((from-01 x1 x2) t)
                           ((from-01 y1 y2) t)])
                        vs1 vs2)
                stroke ((morph (p1 :stroke) (p2 :stroke)) t)
                fill   ((morph (p1 :fill)   (p2 :fill))   t)]
            {:type     :polygon
             :vertices vs
             :stroke   stroke
             :fill     fill}))))

  #_(letfn [(approx-circle [c]
              (-> (repoly 15)
                  (polygon :fill (c :fill) :stroke (c :stroke))))]
      (defmethod morph [:polygon :circle] [p c]
        (let [m (morph p (approx-circle c))]
          (fn [t]
            (case t
              0 p, 1 c, (m t))))))

  #_(defmethod morph [:circle :polygon] [c p] (rev-anime (morph p c)))
  #_(defmethod morph [:circle :circle] [c c+]
      (fn [t]
        (let [stroke ((morph (c :stroke) (c+ :stroke)) t)
              fill   ((morph (c :fill)   (c+ :fill))   t)]
          {:type :circle :stroke stroke :fill fill}))))

(do ;; Layout & Configuration
  ;; Configuration: a map of objects, indexed by ids
  ;; Object: a frame and a list of shapes
  (defn grid [bounding-frame rows cols]
    ;; Divide "bounding-frame" into a rows×cols grid
    (fn core
      ([row col] (core row col {}))
      ([row col {:keys [rspan   cspan]
                 :or   {rspan 1 cspan 1}}]
       ;; Returns a frame at given location
       (-> bounding-frame
           (.scale (/ 1 cols) (/ 1 rows))
           (.translate col row)
           (.scale cspan rspan)))))

  (defn flex
    "Uni-directional frame manager like flexbox"
    [flow-dir bounding-frame size]
    {:pre [(#{:x :y} flow-dir)]}
    (fn core
      ([id] (core id 1))
      ([id span]
       (case flow-dir
         :x ((grid bounding-frame 1 size) 0 id {:cspan span})
         :y ((grid bounding-frame size 1) id 0 {:rspan span})))))

  (do  ;; Effects on shapes & objects
    (defn fade [shape]
      (-> shape
          (assoc-in [:fill :a]   0)
          (assoc-in [:stroke :a] 0)))

    (defn fade-obj [obj]
      (update obj :shapes (fn [shapes] (map fade shapes)))))

  (defn chain [bindings]
    "`bindings` binds id's to functions of bindings.
    Example: (def c (chain {:a (fn [_] 0), :b (fn [m] (inc (m :a)))}))
    (c :b) ➾ 1"
    (def f
      (-> (fn [name] ((bindings name) f))
          memoize))
    f)

  (defn morph-shapes
    "`ss1` fades out, and `ss2` fades in"
    [ss1 ss2]
    (cond (= ss1 ss2) (fn [t] ss1)
          :else (let [ms (concat (for [s ss1] (morph s (fade s)))
                                 (for [s ss2] (morph (fade s) s)))]
                  (fn [t] (for [m ms] (m t))))))

  (defn morph-obj [o1 o2]
    (let [mf (morph        (o1 :frame)  (o2 :frame))
          ms (morph-shapes (o1 :shapes) (o2 :shapes))]
      (fn [t] {:frame (mf t) :shapes (ms t)})))

  (defn morph-conf
    "Morph two configurations (`c2` is allowed to be a function)"
    ([c1 c2] (morph-conf c1 c2 identity))
    ([c1 c2 mapping]
     (let [m (->> (for [id (keys c1)]
                    (let [id+ (mapping id)]
                      (cond
                        ;; We can guard against null, but not all cases
                        (not id+)
                        (throw (str "target does not exist for id " id))

                        (not (c2 id+))
                        (throw (str "target id " id+ " does not exist"))

                        :else
                        [id (morph-obj (c1 id) (c2 id+))])))
                  (into {}))]
       ;; Now `m` contains morphs, we feed them time to get back objects
       (fn [t] (->> (for [[k v] m] [k (v t)])
                    (into {})))))))

(do ;; Things to do with the context
  (defn lift-ctx
    "Safely u/transform the context according to `setup` to perform the function `body`"
    [ctx setup body]
    (.save ctx) (setup) (let [out (body)] (.restore ctx) out))

  (defn fill-background [ctx color]
    (lift-ctx
     ctx
     (fn [] (set! (.-fillStyle ctx) (css color)))
     (fn [] (.fillRect ctx 0 0
                       (-> ctx .-canvas .-width)
                       (-> ctx .-canvas .-height))))))
