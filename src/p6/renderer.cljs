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

;; reduce-rendert can definitely be a conft
;; lam Syntax: The identity function is (lam [x] x)
(defn anon?
  "Check if a variable/parameter is meant to be rendered anonymously"
  [exp] (= (-> exp :name str first) \_))

(defn parse
  "Parse hand-written lam-expression
   Types of expressions: var, global, param, abs(traction), app(lication)
   `env` maps parameters to gen-symbols
   (parse '((lam [x] x) a)) ➾
   {:type :app
    :rator {:type :abs, :params ({:name x, :sym G__315})
            :body {:type :var, :name x, :ref G__315}}
    :rands ({:type :global, :name a})}"
  ([exp] (parse exp {}))
  ([exp env]
   (cond (symbol? exp)
         (if-let [lu (env exp)]
           {:type :var, :name exp, :ref lu}
           {:type :global, :name exp})

         (= (first exp) 'lam)
         (let [[_lam params body] exp]
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

(defn ref-lam [exp path]
  (cond (empty? path) exp
        :else (let [[a & d] path
                    ;; The path is written exactly based on component's name
                    exp+ (exp a)]
                (assert exp+ (str "Path does not exist " path))
                (ref-lam exp+ d))))

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

(defn subst
  "`env` is the map of symbols → expressions to be substituted"
  [exp env]
  (case (:type exp)
    :global exp
    :var (let [lu (env (:ref exp))]
           ;; Copy the expression before substituting
           (cond lu (copy-lam lu)
                 :else exp))
    :abs (update exp :body #(subst % env))
    :app (-> exp
             (update :rator #(subst % env))
             (update :rands (fn [rands]
                              (map #(subst % env) rands))))))

(declare lam-conf)
(defn change-prefix [pre pre+ path]
  (concat pre+ (drop (count pre) path)))

(def globals
  ;; Each global name is mapped to a map with these keys.
  ;; macro (if it's a macro): given its arguments, returns what it expands to
  ;; def (if it's a definition): returns what the definition of the name
  ;; conf (opt): given its arguments, frame, path and env, returns its render config
  {'True {:def (parse '(lam [_x _y] _x))}
   'False {:def (parse '(lam [_x _y] _y))}
   'and {:def (parse '(lam [_x _y] (if _x _y False)))}
   'or {:def (parse '(lam [_x _y] (if _x True _y)))}
   'loop {:def (parse '((lam [_x] (_x _x)) (lam [_x] (_x _x))))}
   'if {:macro (fn [test then else]
                 {:type :app, :rator test, :rands [then else]})
        :conf (fn [[test then else] frame {:keys [path env]}]
                (let [c (-> (lam-conf
                             {:type :app :rator test :rands [then else]}
                             frame
                             {:path path, :env env}))
                      p #(concat path %)
                      c (update-in  ;; Add green + red "lights"
                         c [(p [:rator]) :shapes]
                         concat
                         [(u/line [1 0.1] [1 0.4]
                                  {:stroke u/green :line-width 5})
                          (u/line [1 0.6] [1 0.9]
                                  {:stroke u/red :line-width 5})])]
                  (->> (for [[k v] c]
                         [(cond
                            (u/prefix? (p [:rator]) k)
                            (change-prefix (p [:rator]) (p [:rands 0]) k)
                            (u/prefix? (p [:rands 0]) k)
                            (change-prefix (p [:rands 0]) (p [:rands 1]) k)
                            (u/prefix? (p [:rands 1]) k)
                            (change-prefix (p [:rands 1]) (p [:rands 2]) k)
                            :else k)
                          v])
                       (into {}))))}})

(defn reduce-lam
  "(lam [x] x) ➾ (lam [x] x)
  (((lam [x] (x x)) (lam [x] (x x))) ➾ itself)
  Returns both `exp` and the `path` of the reduction (if happened)
  Also return `prim?` when the reduction is primitive
  If no reduction is possible, returns `nil`.
  We try to be liberal for testing: the worst result is `nil`, not error."
  ([exp] (reduce-lam exp []))
  ([exp path]
   (case (:type exp)
     (:var :abs) nil
     :global (if-let [d (-> exp :name globals :def)]  ;; A global definition
               {:exp d, :path path, :prim? true}
               nil)
     :app (let [{:keys [rator rands]} exp]
            (case (:type rator)
              :var (throw (str "How can a variable ever be an operator?" rator))
              ;; Global operator: some primitive stuff needs to happen
              :global
              (let [g (-> rator :name globals)]
                (if-let [m (:macro g)]  ;; Handle macro
                  {:exp (apply m rands), :path path, :prim? true}
                  (if-let [d (:def g)]  ;; Just a normal global definition
                    {:exp (assoc exp :rator d)
                     :path (concat path [:rator]), :prim? true}
                    nil)))

              ;; Abstraction: this is vanilla application
              :abs (let [{:keys [params body]} rator
                         env (->> (for [[param arg]
                                        (map vector params rands)]
                                    [(:sym param) arg])
                                  (into {}))]
                     (assert (= (count params) (count rands))
                             (str "Arity mismatch: " exp))
                     {:exp (subst body env)
                      :path path
                      :prim? false})
              ;; Application: might be reducible, let's see...
              :app (if-let [red
                            (reduce-lam rator (concat path [:rator]))]
                     ;; Note that if the primitive-ness is propagated up
                     (assoc red
                            :exp (assoc exp :rator (:exp red)))
                     nil))))))

(do  ;; Lambda rendering parameters
  (do  ;; The Orange-Memphis colors
    (def om-orange (u/hsl 21,  78, 59))
    (def om-yellow (u/hsl 41,  99, 68))
    (def om-pink (u/hsl 25,  00, 78))
    (def om-white (u/hsl 30,  83, 88))
    (def om-brown (u/hsl 359, 50, 22)))

  (def lam-pad 0.9)
  (def var-style {:stroke om-yellow :fill u/transparent})
  (defn lam-var-shapes [name]
    [(u/text name var-style)])
  (def lam-anon-shapes
    [(u/circ 0.5 0.5 0.1 var-style)])
  (def lam-param-shapes lam-var-shapes)
  (defn lam-global-shapes [sym]
    [(u/text sym {:stroke om-white :fill om-white})])
  (def lam-ref-shapes
    [(u/line [0 0] [1 1] {:stroke om-yellow})]))

(defn lam-conf
  "Configuration of `exp`, with ids relative to `path`
  physically positioned relative to `frame`
  `env` maps binding references to parameters' frames"
  ([exp frame] (lam-conf exp frame {}))
  ([exp frame {:keys [path env] :or {path [], env {}}}]
   (case (:type exp)
     :global  ;; Global definition (not macro)
     {path {:frame frame
            :shapes (lam-global-shapes (:name exp))}}
     :var {path (if (anon? exp) {:frame (u/force-square frame)
                                 :shapes lam-anon-shapes}
                    {:frame frame
                     :shapes (lam-var-shapes (:name exp))})
           ;; The reference of the variable
           (concat path [:ref])
           {:frame
            (let [[src-x src-y] (u/transform frame [0.5 0.5])
                  [tar-x tar-y] (u/transform (env (:ref exp))
                                             [0.5 0.5])]
              (-> (u/dmat)
                  (.translate src-x src-y)
                  (.scale (- tar-x src-x) (- tar-y src-y))))
            ;; Shape: a line from [0 0] to [1 1]
            :shapes lam-ref-shapes}}

     :abs (let [{:keys [params body]} exp
                division 0.2  ;; param / (the whole frame)
                params-frame (.scale frame 1 division)
                body-frame (-> (.translate frame 0 division)
                               (.scale 1 (- 1 division))
                               (u/pad-frame lam-pad))
                params-conf
                (->> (let  ;; Config of parameters (to which we refer in the body)
                         [scale (cond (= 0 (count params)) nil
                                      :else (/ 1 (count params)))]
                       (for
                           [[param i] (u/enumerate params)]
                         {(concat path [:params i])
                          {:frame (-> params-frame
                                      (.scale scale 1)
                                      (.translate i 0)
                                      (u/pad-frame lam-pad)
                                      (#(if (anon? param) (u/force-square %)
                                            %)))
                           :shapes (if (anon? param) lam-anon-shapes
                                       (lam-param-shapes (:name param)))}}))
                     (apply merge))]
            (merge
             {;; The outline
              path {:frame frame
                    :shapes [(u/line [0 division] [1 division]
                                     {:stroke om-white, :line-width 5})]}}
             (;; Config of the body
              lam-conf body, body-frame
              {:env (merge env
                           (->> (for [[param i] (u/enumerate params)]
                                  [(:sym param) (-> (concat path [:params i])
                                                    params-conf
                                                    :frame)])
                                (into {})))
               :path (concat path [:body])})

             params-conf))

     :app
     (let [{:keys [rator rands]} exp]
       (cond
         ;; Delegation to the globals' rendering function (if exists)
         (and (= (:type rator) :global)
              (-> rator :name globals :conf))
         ((-> rator :name globals :conf)
          rands
          frame
          {:path path :env env})

         :else
         (let [division 0.5  ;; How much is rator, compared to the whole frame
               rator-path (concat path [:rator])
               rator-frame (-> frame
                               (.scale division 1)
                               (u/pad-frame lam-pad))
               rands-path (concat path [:rands])
               rands-frame (-> frame
                               (.translate division 0)
                               (.scale (- 1 division) 1)
                               ;; (u/pad-frame lam-pad)  Not sure if this is needed, since individual operands are already padded
                               )]
           (apply merge
                  {;; The outline
                   path {:frame frame
                         :shapes [(u/line [division 0] [division 1]
                                          {:stroke om-white, :line-width 5})]}}
                  (;; Config of the rator
                   lam-conf rator, rator-frame, {:path rator-path, :env env})
                  (let  ;; Configs of the rands
                      [scale (/ 1 (count rands))]
                    (for
                        [[rand i] (map vector rands (range))]
                      (let [rand-path (concat rands-path [i])
                            rand-frame (-> rands-frame
                                           (.scale 1 scale)
                                           (.translate 0 i)
                                           (u/pad-frame lam-pad))]
                        (lam-conf rand, rand-frame
                                  {:path rand-path, :env env})))))))))))

(defn reduce-objst
  "time → objects showing the reduction of `exp`
  `red` is the reducrion of `exp`
  `tween` is gonna be applied to all phases"
  ([exp red frame] (reduce-objst exp red frame {}))
  ([exp red frame {tween :tween :or {tween identity}}]
   (let [ec (lam-conf exp frame)
         {exp+ :exp, path :path, prim? :prim?} red
         ec+ (lam-conf exp+ frame)]
     (cond
       prim?  ;; Primitive expansion
       (let [l (concat  ;; Morph all the objects first
                (for [obj (vals ec)]  ;; The sugar fades out
                  (u/morph-obj obj (u/fade-obj obj)))
                (for [obj (vals ec+)]  ;; The real function fades in
                  (u/morph-obj (u/fade-obj obj) obj)))]
         (fn [t]
           (let [t (tween t)]  ;; Retrieve the objects
             (for [ot l] (ot t)))))

       :else  ;; Normal application
       (let [rands (ref-lam exp (concat path [:rands]))
             ;; Map each var path under rator-body to its param path
             body-refs
             (letfn [(lam-refs ;; Maps of var-paths to param-paths in `exp`
                       ([exp] (lam-refs exp {} []))
                       ([exp env path]
                        (case (:type exp)
                          :global {}
                          :var (if-let [lu (env (:ref exp))] {path lu}
                                       {})
                          :abs (let [;; Abstraction updates the environment
                                     env+ (->> (for [[param i]
                                                     (u/enumerate (:params exp))]
                                                 [(:sym param)
                                                  (concat path [:params i])])
                                               (into {}))]
                                 (lam-refs (:body exp), (merge env env+)
                                           (concat path [:body])))
                          :app (;; Basically recurse down and merge everything together
                                apply merge
                                (lam-refs (:rator exp), env, (concat path [:rator]))
                                (for [[rand i] (u/enumerate (:rands exp))]
                                  (lam-refs rand, env, (concat path [:rands i])))))))]
               (let [rpath (concat path [:rator])]
                 (-> exp
                     (ref-lam rpath)
                     lam-refs
                     ;; Note that we have to add back the relative path
                     (#(->> (for [[k v] %]
                              [(concat rpath k) (concat rpath v)])
                            (into {}))))))

             replaced-refs (for [[k v] body-refs
                                 :when (u/prefix? (concat path
                                                          [:rator :params])
                                                  v)]
                             [k v])

             ;; Animation scheme
             ;; Phase 1: Morph `ec` → `conf1`
             ;; `conf1`: From `ec`, move rands to rator-params (keep params!)
             ;; Phase 2: Morph `conf2` → `conf3`, with guidelines
             ;; `conf3`: From `ec+`, `path` is rendered in rator-body of `ec`
             ;; `conf2`: From `conf3`, with replaced variables moved to corresponding operands in `conf1`
             ;; Phase 3 (Final): Morph `conf3` to `ec+`

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
                    (lam-conf (ref-lam exp+ path)
                              (:frame (ec (concat path [:rator :body])))
                              {:path path}))

             ;; `conf2`: This is the hairiest part
             conf2 conf3
             conf2 (reduce  ;; Move replaced vars to their params
                    (fn [conf replaced-var]
                      (let [;; the rator-body part isn't there anymore
                            replaced-var+
                            (concat path
                                    (drop (count (concat path [:rator :body]))
                                          replaced-var))
                            param-path (body-refs replaced-var)
                            i (last param-path)
                            ;; Corresponding operand's path (to refer in `conf1`)
                            ;; We use `conf1` because the substitution is already done
                            rand-path (-> param-path
                                          (#(drop-last 3 %))
                                          (concat [:rands i]))]
                        (->> (for [[k v] conf]
                               (cond (u/prefix? replaced-var+ k)
                                     (do
                                       [k (conf1
                                           (concat rand-path
                                                   (drop (count replaced-var+) k)))])

                                     :else [k v]))
                             (into {}))))
                    conf2
                    (for [[k v] replaced-refs] k))
             ]
         (u/series
          [;; Phase 1
           (let [m (;; Just the operand moving
                    u/morph-conf ec conf1)]
             (fn [t] (-> t tween m vals)))
           1]
          [;; Phase 2
           (let [guidelines (for [[k v] replaced-refs]
                              {:shapes [(u/line (u/transform (:frame (ec k)) [0.5 0.5])
                                                (u/transform (:frame (ec v)) [0.5 0.5])
                                                {:stroke u/red, :line-width 2})]
                               :frame (u/dmat)})
                 c (u/morph-conf conf2 conf3)]
             (fn [t]
               (-> t tween c vals (concat guidelines))))
           1]
          [;; Phase 3
           (fn [t]
             (-> t tween ((u/morph-conf conf3 ec+)) vals))
           0.5]))))))

(defn lam-reductions
  "Reduce from `exp` to normal form, or hits `limit`
  Type: a list of expressions"
  ([exp] (lam-reductions exp 10))
  ([exp limit]
   (cond (= limit 0) []
         :else (let [{exp+ :exp :as red} (reduce-lam exp)]
                 (cond exp+ (concat [red] (lam-reductions exp+ (dec limit)))
                       :else [])))))

(defn reduce! [exp]
  (let [exp (parse exp)
        reds (lam-reductions exp)
        objs (for [[red i] (u/enumerate reds)]
               (reduce-objst (;; The starting exp
                              if (= i 0) exp (:exp (nth reds (dec i))))
                             red
                             @a/the-frame
                             {:tween u/quad-out}))]
    (swap! (-> a/t-control :state)
           assoc :cycle (* 2 (count reds)))

    (a/set-draw!
     (fn [params]
       (u/fill-background a/ctx om-brown)
       (let [t (params "t")]
         ((apply u/series
                 (for [[ct i] (u/enumerate objs)]
                   [(fn [t] (u/render-objs a/ctx (ct t)))
                    (if (:prim? (nth reds i)) 0.2
                        1)]))
          t))))))

(defn draw-lam! [exp]
  (let [exp (parse exp)
        c (lam-conf exp @a/the-frame)]
    (a/set-draw!
     (fn [params]
       (u/fill-background a/ctx om-brown)
       (u/render-conf a/ctx c)))))

(reduce! '(or True False))

;; Get custom appearance for global defs: like eg a tickmark for "true"
;; I need types real bad: vector type hint for path
;; Make a "capture" button
;; Try resizing the lambda parts
;; Draw directly from vertices, to not lose structure of polygons. The drawing gets more complex, but it's a price you pay for richer structures
;; Implement 2D mouse control
