(ns cel-aut.history
  "Management of undo-redo")

(defn history []
  {:elements [] :current 0})

(defn push [h st]
  (-> h
      (as-> it
        (update it :elements #(take (:current it) %)))
      (update :elements vec)
      (update :elements conj st)
      (update :current inc)))

(defn undo [{:keys [current] :as h}]
  (if (> current 1)
    (update h :current dec)
    h))

(defn redo [{:keys [elements current] :as h}]
  (if (>= current (count elements))
    (assoc h :current (count elements))
    (update h :current inc)))

(defn head [{:keys [elements current]}]
  (last (take current elements)))

(defn can-undo? [h]
  (> (:current h) 0))

(defn can-redo? [{:keys [elements current]}]
  (> (count elements) current))
