(ns p6.renderer
  (:require
   [cljs.reader :refer [read-string]]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]]
   [clojure.string :as str]
   [cljs.pprint :refer [cl-format]]

   [p6.util :as u]
   [p6.app :as a])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

;; beta-reduce-rendert can definitely be a conft
;; λ Syntax: The identity function is (λ [x] x)
(defn parse
  "Parse hand-written λ-expression
    Types of expressions: var, global, param, abs(traction), app(lication)
    `env` maps parameters to gen-symbols
    (parse '((λ [x] x) a)) ➾
    {:type :app
     :rator {:type :abs, :params ({:name x, :sym G__315})
             :body {:type :var, :name x, :ref G__315}}
     :rands ({:type :global, :name a})}"
  ([exp] (parse exp {}))
  ([exp env]
   (cond (symbol? exp)
         (let [lu (env exp)]
           (cond lu {:type :var, :name exp, :ref lu}
                 :else {:type :global, :name exp}))

         (= (first exp) 'λ)
         (let [[_λ params body] exp]
           (assert (and params body)
                   (str "Fowl expression: " exp))
           (let [;; Generate new symbols for new params
                 env+ (->> (for [param params] [param (gensym)])
                           (into {}))]
             {:type :abs,
              :params (for [[param i] (u/enumerate params)]
                        {:name param, :sym (env+ param)}),
              :body (parse body, (merge env env+))}))

         :else
         (let [[rator & rands] exp]
           (assert rator
                   (str "Fowl expression: " exp))
           {:type :app
            :rator (parse rator env)
            :rands (for [rand rands] (parse rand env))}))))

(defn lam-ref [exp path]
  (cond (empty? path) exp
        :else (let [[a & d] path
                    ;; The path is written exactly based on component's name
                    exp+ (exp a)]
                (assert exp+ (str "Path does not exist " path))
                (lam-ref exp+ d))))

(defn copy-lam
  "Freshen all internal parameters in `exp`, used in substitution
    `env` maps symbols to symbols"
  ([exp] (copy-lam exp {}))
  ([exp env]
   (case (:type exp)
     :global exp
     :var (let [ref (exp :ref), ref+ (env ref)]
            ;; If it refers to an internal local var, duplicate it!
            (assoc exp :ref (or ref+ ref)))
     :abs (let [params (exp :params)
                rename (->> (for [param params]
                              [(param :sym) (gensym)])
                            (into {}))]
            (-> exp
                (assoc :params (for [param params]
                                 (update param :sym rename)))
                (assoc :body (copy-lam (exp :body) (merge env rename)))))
     :app (-> exp
              (update :rator #(copy-lam % env))
              (update :rands #(for [rand %]
                                (copy-lam rand env)))))))

(defn subst-lam
  "`env` is the map of symbols → expressions to be substituted"
  [exp env]
  (case (:type exp)
    :global exp
    :var (let [lu (env (:ref exp))]
           ;; Copy the expression before substituting
           (cond lu (copy-lam lu)
                 :else exp))
    :abs (update exp :body #(subst-lam % env))
    :app (-> exp
             (update :rator #(subst-lam % env))
             (update :rands (fn [rands]
                              (map #(subst-lam % env) rands))))))

(defn beta-reduce
  "(λ [x] x) ➾ (λ [x] x)
     (((λ [x] (x x)) (λ [x] (x x))) ➾ itself)
     Returns both `exp` and the `path` of the reduction (if happened)
     If no reduction is possible, returns `nil`"
  ([exp] (beta-reduce exp []))
  ([exp path]
   (case (:type exp)
     (:global :var :abs) nil
     :app (let [{:keys [rator rands]} exp]
            (case (:type rator)
              (:global :var) nil
              ;; Abstraction: this is the direct application case
              :abs (let [{:keys [params body]} rator
                         env (->> (for [[param arg]
                                        (map vector params rands)]
                                    [(:sym param) arg])
                                  (into {}))]
                     {:exp (subst-lam body env)
                      :path path})
              ;; Application: might be reducible, let's see...
              :app (let [{rator+ :exp, path+ :path}
                         (beta-reduce rator (concat path [:rator]))]
                     (cond rator+ {:exp (assoc exp :rator rator+)
                                   :path path+}
                           :else nil)))))))

(do  ;; Lambda rendering parameters
  (do  ;; The Orange-Memphis colors
    (def om-orange (u/hsl 21,  78, 59))
    (def om-yellow (u/hsl 41,  99, 68))
    (def om-pink (u/hsl 25,  00, 78))
    (def om-white (u/hsl 30,  83, 88))
    (def om-brown (u/hsl 359, 50, 22)))

  (def lam-pad 0.9)
  (defn lam-var-shapes [name]
    [(u/text name {:stroke om-yellow :fill u/transparent})])
  (def lam-param-shapes lam-var-shapes)
  (defn lam-global-shapes [name]
    [(u/text name {:stroke om-white :fill om-white})]))

(defn lam-conf
  "Configuration of the λ-expression at path `path` under `exp`,
    physically positioned relative to `frame`
    Note: References are not (and cannot be) included"
  ([exp frame] (lam-conf exp frame {}))
  ([exp frame {:keys [path] :or {path []}}]
   (case (:type exp)
     :global {path {:frame frame
                    :shapes (lam-global-shapes (:name exp))}}
     :var {path {:frame frame
                 :shapes (lam-var-shapes (:name exp))}}

     :abs (let [{:keys [params body]} exp]
            (let [division 0.2  ;; param / (the whole frame)
                  params-frame (.scale frame 1 division)
                  body-frame (-> (.translate frame 0 division)
                                 (.scale 1 (- 1 division))
                                 (u/pad-frame lam-pad))]
              (apply merge
                     {path {:frame frame
                            :shapes [(u/line [0 division] [1 division]
                                             {:stroke om-white
                                              :line-width 5})]}}
                     (;; Config of the body
                      lam-conf body body-frame {:path (concat path [:body])})

                     (let [scale (cond (= 0 (count params)) nil
                                       :else (/ 1 (count params)))]
                       (for  ;; Config of the parameters
                           [[param i] (u/enumerate params)]
                         {(concat path [:params i])
                          {:frame (-> params-frame
                                      (.scale scale 1)
                                      (.translate i 0)
                                      (u/pad-frame lam-pad))
                           :shapes (lam-param-shapes (:name param))}})))))

     :app (let [{:keys [rator rands]} exp]
            (let [division 0.5  ;; How much is rator, compared to the whole frame
                  rator-path (concat path [:rator])
                  rator-frame (-> frame
                                  (.scale division 1)
                                  (u/pad-frame lam-pad))
                  rands-path (concat path [:rands])
                  rands-frame (-> frame
                                  (.translate division 0)
                                  (.scale (- 1 division) 1)
                                  (u/pad-frame lam-pad))]
              (apply merge
                     {path {:frame frame
                            :shapes [(u/line [division 0] [division 1]
                                             {:stroke om-white, :line-width 5})]}}
                     (lam-conf rator, rator-frame, {:path rator-path})
                     (let [scale (/ 1 (count rands))]
                       (for  ;; Drawing the rands
                           [[rand i] (map vector rands (range))]
                         (let [rand-path (concat rands-path [i])
                               rand-frame (-> rands-frame
                                              (.scale 1 scale)
                                              (.translate 0 i)
                                              (u/pad-frame lam-pad))]
                           (lam-conf rand, rand-frame
                                     {:path rand-path}))))))))))

(defn lam-refs
  "Returns a mapping of var-paths to param-paths in `exp`"
  ([exp] (lam-refs exp {} []))
  ([exp env path]
   (case (:type exp)
     :global {}
     :var {path (env (:ref exp))}
     :abs (let [;; Abstraction updates the environment
                env+ (->> (for [[param i] (u/enumerate (:params exp))]
                            [(:sym param) (concat path [:params i])])
                          (into {}))]
            (lam-refs (:body exp), (merge env env+)
                      (concat path [:body])))
     :app (;; Basically recurse down and merge everything together
           apply merge
           (lam-refs (:rator exp), env, (concat path [:rator]))
           (for [[rand i] (u/enumerate (:rands exp))]
             (lam-refs rand, env, (concat path [:rands i])))))))

(defn render-refs
  ([conf refs] (render-refs conf refs {:stroke om-yellow}))
  ([conf refs style]
   (doseq [[var-path param-path] refs]
     (let [var-frame (:frame (conf var-path))
           param-frame (:frame (conf param-path))]
       (a/render-conf
        {(gensym)
         {:frame (u/dmat)  ;; This is absolute, not relative to a frame
          :shapes [(u/line (u/transform var-frame [0.5 0.5])
                           (u/transform param-frame [0.5 0.5])
                           style)]}})))))

(defn render-lam [exp frame]
  (let [conf (lam-conf exp frame)]
    (a/render-conf conf)
    (render-refs conf (lam-refs exp))))

(defn beta-reduce-rendert
  "time → rendering of the beta reduction of `exp`
    `tween` is gonna be applied to all phases"
  ([exp frame] (beta-reduce-rendert exp frame {}))
  ([exp frame {tween :tween :or {tween identity}}]
   (let [ec (lam-conf exp frame)
         erefs (lam-refs exp)
         {exp+ :exp path :path} (beta-reduce exp)
         erefs+ (lam-refs exp+)
         ec+ (lam-conf exp+ frame)

         rands (lam-ref exp (concat path [:rands]))
         ;; Map each var under rator-body to its param path
         usage (->> (lam-refs exp)
                    (filter (fn [[k v]]
                              (u/prefix? (concat path [:rator :params]) v)))
                    (into {}))
         ;; Vars that will be replaced in this β-reduction
         replaced-vars (->> (keys usage) (into #{}))

         ;; Animation scheme
         ;; Phase 1: Morph `ec` → `conf1`, render `erefs`
         ;; `conf1`: From `ec`, move rands to rator-params (keep params!)
         ;; Phase 2: Morph `conf2` → `conf3`, render `erefs+`, with guidelines
         ;; `conf3`: From `ec+`, `path` is rendered in rator-body of `ec`
         ;; `conf2`: From `conf3`, with replaced variables moved to corresponding operands in `conf1`
         ;; Phase 3 (Final): Morph `conf3` to `ec+`, rendering `erefs+`

         ;; `conf1`
         conf1 ec
         conf1 (apply merge
                      conf1
                      (for [[rand i] (u/enumerate rands)]
                        (let [rand-path (concat path [:rands i])
                              ;; The corresponding param
                              rparam-path (concat path [:rator :params i])]
                          (;; Config of `rand`, if it were moved to the corresponding param
                           lam-conf rand
                           (;; The frame of the corresponding param
                            :frame (ec rparam-path))
                           {;; The config is relative to this path
                            :path rand-path}))))

         ;; `conf3`
         conf3 ec+
         conf3 (merge
                conf3
                (lam-conf (lam-ref exp+ path)
                          (:frame (ec (concat path [:rator :body])))
                          {:path path}))

         ;; `conf2`: This is the hairiest part
         conf2 conf3
         conf2 (reduce
                (fn [conf replaced-var]
                  (let [;; the rator-body part doesn't exist anymore
                        replaced-var+
                        (concat path
                                (drop (count (concat path [:rator :body]))
                                      replaced-var))
                        param-path (erefs replaced-var)
                        i (last param-path)
                        ;; Corresponding operand's path
                        rand-path (-> param-path
                                      (#(drop-last 3 %))
                                      (concat [:rands i]))]
                    (->> (for [[k v] conf]
                           (cond (u/prefix? replaced-var+ k)
                                 [k (conf1
                                     (concat rand-path
                                             (drop (count replaced-var+) k)))]
                                 :else [k v]))
                         (into {}))))
                conf2
                replaced-vars)
         ]
     (u/series
      [;; Phase 1
       (let [ct (;; Just the operand moving
                 u/morph-conf ec conf1)]
         (fn [t]
           (let [t (tween t), c (ct t)]
             (a/render-conf c)
             (render-refs c erefs))))
       1]
      [;; Phase 2
       (let [ct (u/morph-conf conf2 conf3)
             guides (->> (filter (fn [[k v]]
                                   ;; Retain only those refs in rator-body
                                   (u/prefix? (concat path [:rator :body]) k))
                                 erefs)
                         (into {}))]
         (fn [t]
           (;; Render the guidelines (not time-dependent)
            render-refs ec, guides, {:stroke u/red :line-width 2})
           (let [t (tween t), c (ct t)]
             (a/render-conf c)
             (render-refs c erefs+))))
       1]
      [;; Phase 3
       (let [ct (u/morph-conf conf3 ec+)]
         (fn [t]
           (let [t (tween t), c (ct t)]
             (a/render-conf c)
             (render-refs c erefs+))))
       0.5]))))

(defn beta-reductions
  "Reduce from `exp` to normal form, or hits `limit`"
  ([exp] (beta-reductions exp 10))
  ([exp limit]
   (cond (= limit 0) []
         :else (let [{exp+ :exp} (beta-reduce exp)]
                 (cond exp+ (concat [exp] (beta-reductions exp+ (dec limit)))
                       :else [])))))

(let [exp (parse '(((λ [f a] (a f)) (λ [x] x) (λ [x] x))
                   z))
      reds (beta-reductions exp)
      rts (for [e reds]
            (beta-reduce-rendert e a/the-frame {:tween u/quad-out}))]
  (a/set-draw!
   (fn [params]
     (u/fill-background a/ctx om-brown)
     ((apply u/series (for [rt rts] [rt 1]))
      (params "t")))))

;; Try resizing the lambda parts if needed
;; Make a "capture" button
;; Implement frame movement techniques (as in introduction & delay)
;; implement intro/outro frames (in the morph function)
;; Draw directly from vertices, to not lose structure of polygons
;; the drawing gets more complex, but it's a price you pay for richer structures
;; Implement 2D mouse control
