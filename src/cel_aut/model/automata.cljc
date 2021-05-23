(ns cel-aut.model.automata
  (:require [cel-aut.history :as h]))

(defprotocol IDrawable
  (formats [_]
    "Returns the list of formats supported by this Drawable. The first on the list is
    the default one")
  (drawable
    [_]
    [_ fmt]
    "Returns the drawable for the given format or the for the default one if format
    is not provided.
    The concrete type returned will depend on the format. For the format :2d, the
    returned drawble is a map with keys :cols :rows :elemens, where :cols and :rows are
    positive integers and :rows is an array of elements where the first element is the
    element of the first row and first column, the second element the element in the
    first row and second column, ..., the 'number of columns + 1' element is the element
    on the second row and first column, etc"))

(defprotocol IAutomata
  (next-gen
    [a]
    [a num]
    "Calculates and returns num number of generations of the given automata
    (1 if num is not provided)."))

(defprotocol IEditable
  (cell-states [_]
    "Returns the list of states for a cell, as a vector")
  (cycle-cell [_ x y]
    "Cycles the value of the given cell to the next state from the list of
     states, in the given order. The implementation can skip the values that are not
     valid"))

(defprotocol IHistory
  (redo [_]
    "Redos the last undone action. Does nothing if there are no actions to redo")
  (undo [_]
    "Undos the last action. Does nothing is there are no actions to undo")
  (reset [_]
    "Resets the state to the initial (or default) state")
  (total [_]
    "Returns the total number of generations up to the current one, including
    those not kept in the history")
  (can-redo? [_]
    "Returns true is there are actions to redo, false otherwise")
  (can-undo? [_]
    "Returns true is there are actions to undo, false otherwise"))

(defrecord Automata
    [f state cell-states init-st blank-st
     cycle-cell-fn to-drawable-fn formats
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
  (cycle-cell [a x y]
    (update a :state cycle-cell-fn x y))

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
        (as-> it (assoc it :state (h/head (:history it))))))
  (total [a]
    (-> a :history h/total))
  (can-redo? [a]
    (-> a :history h/can-redo?))
  (can-undo? [a]
    (-> a :history h/can-undo?))

  IDrawable
  (formats [_] formats)
  (drawable [_] (to-drawable-fn))
  (drawable [a fmt] (drawable a)))

(defn create-automata
  "Creates an automata from the given parameters.

  :f
      Function that returns the next state from a given one
  :init-st
      Initial state of the automata. The state can be anything, it won't be used.
  :to-drawable-fn
      Function that transforms the state in a represention {:rows n1 :cols n2 :elements v}
  :blank-st
      Blank state, which can be different from the initial state
  :cycle-cell-fn
      Function that takes parameters (state x y) and returns the state modified on
      the cell x y with the next valid value from the list of values
  :cell-states
      Vector with all the possible values for a given cell. Defaults to [true false]
      if not provided
  :undo-levels
      Number of undo levels to keep in the history"
  [{:keys [f init-st to-drawable-fn blank-st cycle-cell-fn cell-states undo-levels]}]
  (let [blank-st      (or blank-st init-st)
        undo-levels   (or undo-levels 0)
        history       (h/init undo-levels init-st)
        formats       [:2d]
        state         init-st
        cell-states   (or cell-states [true false])
        cycle-cell-fn (or cycle-cell-fn identity)]
    (->Automata f state cell-states init-st blank-st
                cycle-cell-fn to-drawable-fn formats
                history)))
