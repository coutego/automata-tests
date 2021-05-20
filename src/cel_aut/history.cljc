(ns cel-aut.history
  "Management of undo-redo")

(defn push
  "Push a new element el at the end of the history"
  [h el]
  (-> h
      (as-> it
        (update it :elements #(take (:current it) %)))
      (update :elements vec)
      (update :elements conj el)
      (update :current inc)))

(defn init
  "Produce a new history, with the initial list of elements els"
  [& els]
  (let [ret (-> {:elements [] :current 0})]
    (reduce push ret els)))

(defn undo
  "Undo one action on the history. Returns the same element is no undo is possible"
  [h]
  (if (> (:current h) 1)
    (update h :current dec)
    h))

(defn redo
  "Redo one action on the history. Returns the same element is no redo is possible"
  [h]
  (let [{:keys [elements current]} h]
    (if (>= current (count elements))
      (assoc h :current (count elements))
      (update h :current inc))))

(defn head
  "Returns the current element, the one at the 'head' of the history"
  [h]
  (let [{:keys [elements current]} h]
    (last (take current elements))))

(defn up-to-head
  "Returns the history, up to the current element (i.e. the ones after the current
  position are not included in the retured history)"
  [h]
  (let [{:keys [elements current]} h]
    (take current elements)))

(defn pos
  "Return the number of elements in the history, up to the current element"
  [h]
  (:current h))

(defn can-undo?
  "Can this history be undone?"
  [h]
  (> (:current h) 1))

(defn can-redo?
  "Can this history be undone?"
  [h]
  (let [{:keys [elements current]} h]
    (> (count elements) current)))
