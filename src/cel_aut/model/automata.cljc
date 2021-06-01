(ns cel-aut.model.automata
  (:require [cel-aut.model.history :as h]))

(defprotocol IDrawable
  (formats [_]
    "Returns the list of formats supported by this Drawable. The first on the list is
    the default one")

  (renderer
    [_]
    [_ fmt]
    "Returns the renderer function for the given format or the for the default one if format
    is not provided.

    The concrete type returned will depend on the format. For the format :2d, the
    returned function accepts one parameter corresponding to the number that is defined as

       p = y + cols * x

    where x and y are the coordinates of the point")

  (geometry [_]
    "Returns the geometry for the given format (or the first one if it's not provided).
    The meaning of geometry is dependent on the format. For the :2d format the geometry
    is a map {:rows n1 :cols n2}"))

(defprotocol IAutomata
  (next-gen
    [a]
    [a num]
    "Calculates and returns num number of generations of the given automata
    (1 if num is not provided)."))

(defprotocol IEditable
  (cell-states [_]
    "Returns the list of states for a cell, as a vector")

  (cycle-cell [_ cell]
    "Cycles the value of the given cell to the next state from the list of
     states, in the given order. The implementation can skip the values that are not
     valid"))

(defprotocol IHistory
  (redo [_]
    "Redoes the last undone action. Does nothing if there are no actions to redo")

  (undo [_]
    "Undoes the last action. Does nothing is there are no actions to undo")

  (reset [_]
    "Resets the state to the initial (or default) state")

  (blank [_]
    "Resets the state to the blank state")

  (total [_]
    "Returns the total number of generations up to the current one, including
    those not kept in the history")

  (can-redo? [_]
    "Returns true is there are actions to redo, false otherwise")

  (can-undo? [_]
    "Returns true is there are actions to undo, false otherwise")

  (undo-levels [_]
    "Returns the number of undo levels in the history")

  (set-undo-levels [_ n]
    "Changes the level of undo-levels in the history to n"))

(defrecord Automata
    [name f state cell-states init-st blank-st
     cycle-cell-fn renderer-fn formats
     history]

  IAutomata
  (next-gen [a]
    (let [new-st (f (:state a))]
      (-> a
          (assoc :state new-st)
          (update :history h/push new-st))))

  (next-gen [a num]
    (let [n (max 1 (int num))]
      (loop [i 0
             a a]
        (if (>= i n)
          a
          (recur (inc i) (next-gen a))))))

  IEditable
  (cell-states [a]
    (:cell-states a))

  (cycle-cell [a cell]
    (let [new-st (cycle-cell-fn (:state a) cell)]
      (-> a
          (update :history h/push new-st)
          (assoc :state new-st))))

  IHistory
  (redo [a]
    (-> a
        (update :history h/redo)
        (as-> it (assoc it :state (h/head (:history it))))))

  (undo [a]
    (-> a
        (update :history h/undo)
        (as-> it (assoc it :state (h/head (:history it))))))

  (reset [a]
    (-> a
        (update :history h/reset)
        (update :history h/push init-st)
        (as-> it (assoc it :state (h/head (:history it))))))

  (blank [a]
    (-> a
        (update :history h/reset)
        (update :history h/push blank-st)
        (as-> it (assoc it :state (h/head (:history it))))))

  (total [a]
    (-> a :history h/total))

  (can-redo? [a]
    (-> a :history h/can-redo?))

  (can-undo? [a]
    (-> a :history h/can-undo?))

  (undo-levels [a]
    (-> a :history :keep))

  (set-undo-levels [a n]
    (let [n (max 0 (int n))]
      (-> a
          (assoc-in [:history :keep] n))))

  IDrawable
  (formats [_] formats)

  (renderer [_] renderer-fn)

  (renderer [a fmt] (renderer a)) ; FIXME

  (geometry [_] {:rows 100 :cols 100})) ; FIXME

(defn create-automata
  "Creates an automata from the given parameters.

  :f
  :   Function that returns the next state from a given one

  :init-st
  :   Initial state of the automata. The state can be anything, it won't be used.

  :to-drawable-fn
  :   Function that transforms the state in a represention {:rows n1 :cols n2 :elements v}

  :blank-st
  :   Blank state, which can be different from the initial state

  :cycle-cell-fn
  :   Function that takes parameters (state x y) and returns the state modified on
      the cell x y with the next valid value from the list of values

  :cell-states
  :   Vector with all the possible values for a given cell. Defaults to [true false]
      if not provided

  :undo-levels
  :   Number of undo levels to keep in the history"
  [{:keys [name f init-st renderer-fn blank-st cycle-cell-fn cell-states undo-levels]}]
  (let [name          (or name "Unknown automata")
        blank-st      (or blank-st init-st)
        undo-levels   (or undo-levels 0)
        history       (h/init undo-levels init-st)
        formats       [:2d]
        state         init-st
        cell-states   (or cell-states [true false])
        cycle-cell-fn (or cycle-cell-fn identity)]

    (->Automata name f state cell-states init-st blank-st
                cycle-cell-fn renderer-fn formats
                history)))
