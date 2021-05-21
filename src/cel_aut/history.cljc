(ns cel-aut.history
  "Management of undo-redo")

(defn- drop-if-needed
  "Drops elements from the history if needed (to limit history to :keep elements)"
  [h]
  (let [ex (dec (- (count (:elements h)) (:keep h)))]
    (println "ex = " ex)
    (if (> ex 0)
      (-> h
          (update :current - ex)
          (update :elements (comp vec #(drop ex %))))
      h)))

(defn push
  "Push a new element el at the end of the history"
  [h el]
  (-> h
      (as-> it
          (update it :elements #(take (:current it) %)))
      (update :elements vec)
      (update :elements conj el)
      (update :current inc)
      (update :total inc)
      drop-if-needed))

(defn init
  "Produce a new history, with the initial list of elements els"
  [keep & els]
  (let [ret (-> {:elements [] :current 0 :total 0 :keep (or keep 100)})]
    (reduce push ret els)))

(defn can-undo?
  "Can this history be undone?"
  [h]
  (> (:current h) 1))

(defn can-redo?
  "Can this history be undone?"
  [h]
  (let [{:keys [elements current]} h]
    (> (count elements) current)))

(defn undo
  "Undo one action on the history. Returns the same element is no undo is possible"
  [h]
  (if (can-undo? h)
    (-> h
      (update :current dec)
      (update :total dec))
    h))

(defn redo
  "Redo one action on the history. Returns the same element is no redo is possible"
  [h]
  (if (can-redo? h)
    (-> h
      (update :current inc)
      (update :total inc))
    (assoc h :current (count (:elements h)))))

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

(defn total
  "Return the number of elements pushed into the history, including those dropped"
  [h]
  (:total h))

(defn current
  "Return the number of elements pushed into the history, not including those dropped"
  [h]
  (:current h))
