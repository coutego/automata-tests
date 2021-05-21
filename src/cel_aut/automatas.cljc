(ns cel-aut.automatas
  "Collection of sample automatas"
  (:require
   [clojure.core.match :refer-macros [match]]))

;; Initial random state. It's a value instead of a fn so reset restores the
;; original state
(def initial-state-rand (mapv #(< (rand-int 10) 3) (range 10000)))

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
    (get state (+ y (* 100 x)))))

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

(defn parity-xy
  "Calculates the value of the cell at x y on the next generation for the parity
  algorithm"
  [n state]
  (= 1 (rem (neighbourgs n state) 2)))

(defn parity
  "Calculates the next state from a given one for the parity algorithm"
  [state]
  (into [] (map-indexed (fn [n _] (parity-xy n state)) state)))

(defn- tap-> [kw v] (tap> {kw v}) v)

(defn- find-ant [st]
  (tap-> :find-ant
         (reduce (fn [acc n]
                   (if (map? n)
                     (reduced [(quot acc 100) (rem acc 100) n])
                     (inc acc)))
                 0
                 st)))

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
  (match val
         {:v v :dir dir} (case dir
                           :up    (if v "hsl(0, 50%, 30%)" "hsl(0, 80%, 50%)")
                           :right (if v "hsl(90, 50%, 30%)" "hsl(90, 80%, 50%)")
                           :down  (if v "hsl(180, 50%, 30%)" "hsl(180, 80%, 50%)")
                           :left  (if v "hsl(240, 50%, 30%)" "hsl(240, 80%, 50%)"))
         false nil
         :else "hsl(40, 20%, 20%)"))

(def automatas
  [{:name "Conway Game of Life"
    :f conway
    :initial-state initial-state-rand}

   {:name "Replicating"
    :f parity
    :initial-state initial-state-e}

   {:name "Langton Ant"
    :f ant
    :initial-state initial-state-ant
    :cell-renderer ant-drawer}])
