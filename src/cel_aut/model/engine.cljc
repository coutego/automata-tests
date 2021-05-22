(ns cel-aut.model.engine
  (:require
   [cel-aut.history :as h]))

(defn ? [m kw & args]
  (let [f (kw m)]
    (if (fn? f)
      (apply f m args)
      m)))

(defn get-number [x min-val]
  (if (not (number? x))
    min-val
    (max min-val (int x))))

(defn create-automata [f to-2d-map initial-state]
  {::next      (fn [a] (update a ::state f))
   ::state     initial-state
   ::to-2d-map to-2d-map})

(defn- historized-next [{::keys [next]}]
  (fn [a]
    (-> a
        (next a)
        (as-> it
            (update it ::history h/push (::state it))))))

(defn historize
  "Given an automata a, creates a new automata with history (undo/redo)
  capabilities"
  [a & [max-undo]]
  (-> a
      (assoc ::history (h/init (max 0 (or max-undo 0))))
      (assoc ::next (historized-next a))
      (assoc ::undo (fn [a]
                      (-> a
                          (update ::history h/undo)
                          (as-> it
                              (assoc it ::state (h/head (::history it)))))))
      (assoc ::redo (fn [a]
                      (-> a
                          (update ::history h/redo)
                          (as-> it
                              (assoc it ::state (h/head (::history it)))))))))

(defn multiple-step-fn [{::keys [next]}]
  (fn [a & [steps]]
    (loop [i (get-number steps 1)
           a a]
      (if (<= i 0)
        a
        (recur (dec i) (next a))))))

(defn make-multiple-step
  "Given an automata a, returns an automata with a next method that accepts
  an optional argument with the number of steps to run"
  [a]
  (assoc a ::next (multiple-step-fn a)))
