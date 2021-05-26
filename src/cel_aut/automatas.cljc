(ns cel-aut.automatas
  "Collection of sample automatas"
  (:require
   [cel-aut.model.automata :as ma]))

#?(:cljs
   (def initial-state-rand-arr
     (let [a (js/Array. 9999)]
       (loop [i 10000]
         (when-not (< i 0)
           (aset a i (< (rand-int 10) 3))
           (recur (dec i))))
       a)))

;; Initial random state. It's a value instead of a fn so reset restores the
;; original state

#?(:clj
    (def initial-state-rand (mapv (fn [_] (< (rand-int 10) 3)) (range 10000)))
   :cljs
    (def initial-state-rand (into [] initial-state-rand-arr)))

;; Letter 'E' state
(def initial-state-e
  (-> (mapv (constantly false) (range 10000))
      (as-> it
          (reduce #(assoc %1 %2 1)
                  it
                  [4849 4850 4851 4949 5049 5050 5051 5149 5249 5250 5251]))))

(def initial-state-ant
  (-> (mapv (constantly false) (range 10000))
      (assoc 5051 {:v true :dir :up})))

(defn v
  "Value at coordinates x y"
  [state x y]
  (let [x (mod x 100)
        y (mod y 100)]
    (if (get state (+ y (* 100 x))) 1 0)))

(defn neighbourgs
  "Sum of the neighbourgs of a cell"
  [n state]
  (let [x           (quot n 100)
        y           (rem n 100)]
    (+ (v state (dec x) (dec y))
       (v state (dec x) y)
       (v state (dec x) (inc y))
       (v state x (dec y))
       (v state x (inc y))
       (v state (inc x) (dec y))
       (v state (inc x) y)
       (v state (inc x) (inc y)))))

(defn conway-xy
  "Calculates the value of the cell at x y on the next generation for the Conway
  algorithm"
  [n val state]
  (let [nei (neighbourgs n state)]
    (cond
      (and val (< nei 2))       false
      (and val (<= 2 nei 3))    true
      (and val (> nei 3))       false
      (and (not val) (= nei 3)) true
      :else                     false)))

(defn conway
  "Calculates the next state from a given one for the Conway algorithm"
  [state]
  (into [] (map-indexed (fn [n v] (conway-xy n v state)) state)))

#?(:cljs
   (defn conway-arr
     "Calculates the next state from a given one for the Conway algorithm"
     [state]
     (let [ret (js/Array. 10000)]
       (dorun
        (for [i (range 10000)]
          (aset ret i (conway-xy i (aget state i) state))))
       ret)))

(defn parity-xy
  "Calculates the value of the cell at x y on the next generation for the parity
  algorithm"
  [n state]
  (= 1 (rem (neighbourgs n state) 2)))

(defn parity
  "Calculates the next state from a given one for the parity algorithm"
  [state]
  (into [] (map-indexed (fn [n _] (parity-xy n state)) state)))

(defn- find-ant [st]
  (reduce (fn [acc n]
            (if (map? n)
              (reduced [(quot acc 100) (rem acc 100) n])
              (inc acc)))
          0
          st))

(defn ant
  "Calculates the next state from a given one for the Langton ant algorith"
  [st]
  (let [[x y {:keys [v dir]}] (find-ant st)
        ndir                  (case dir
                                :up    (if v :left :right)
                                :right (if v :up :down)
                                :down  (if v :right :left)
                                :left  (if v :down :up))
        [nx ny]               (case ndir
                                :up    [(dec x) y]
                                :right [x (inc y)]
                                :down  [(inc x) y]
                                :left  [x (dec y)])]
    (-> st
        (assoc (+ y (* 100 x)) (not v))
        (update (+ ny (* 100 nx)) (fn [v] {:v v :dir ndir})))))

(defn ant-drawer [val]
  (cond
    (map? val)
    (let [{v :v dir :dir} val]
      (case dir
           :up    (if v "hsl(0, 50%, 30%)" "hsl(0, 80%, 50%)")
           :right (if v "hsl(90, 50%, 30%)" "hsl(90, 80%, 50%)")
           :down  (if v "hsl(180, 50%, 30%)" "hsl(180, 80%, 50%)")
           :left  (if v "hsl(240, 50%, 30%)" "hsl(240, 80%, 50%)")))

    (= false val) "hsl(40, 20%, 90%)"
    :else "hsl(40, 50%, 20%)"))

(def automatas-model
  (mapv
   ma/create-automata
   [{:name          "Conway Game of life"
     :f             conway
     :init-st       initial-state-rand
     :blank-st      (mapv (fn [_] false) (range 10000))
     :cycle-cell-fn not
     :renderer-fn   {true "hsl(40, 10%, 10%)" false "hsl(40, 10%, 90%)"}
     :undo-levels   100}
    #?(:cljs
       {:name          "Conway Game of life (Array)"
        :f             conway-arr
        :init-st       initial-state-rand-arr
        :blank-st      (clj->js (map (fn [_] false) (range 10000)))
        :cycle-cell-fn not
        :renderer-fn   {true "hsl(40, 10%, 10%)" false "hsl(40, 10%, 90%)"}
        :undo-levels   100})
    {:name          "Self Replicating"
     :f             parity
     :init-st       initial-state-e
     :blank-st      (mapv (fn [_] false) (range 10000))
     :cycle-cell-fn not
     :renderer-fn   {true "hsl(40, 10%, 10%)" false "hsl(40, 10%, 90%)"}
     :undo-levels   100}
    {:name        "Langton Ant"
     :f           ant
     :init-st     initial-state-ant
     :cycle-cell-fn not
     :blank-st    (mapv (fn [_] false) (range 10000))
     :renderer-fn ant-drawer
     :undo-levels 100}]))

(defn benchmark [f is & [times]]
  (with-out-str
    (time
     (loop [i 0
            st is]
       (when (< i (or times 1000))
         (recur (inc i) (f st)))))))
