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

    The returned function accepts two parameters. The first one is the internal state of
    the automata. The second parameter will depend on the format.

    For the format :2d, the parameter is the number p defined as:

       p = y + cols * x

    where x and y are the coordinates of the point and returns the color to render")

  (state [_]
    "Returns (a copy of) the internal state of the drawable")

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

(deftype Automata [m]
  IAutomata
  (next-gen [_]
    (let [new-st ((:f m) (:state m))]
      (-> m
          (assoc :state new-st)
          (update :history h/push new-st)
          Automata.)))

  (next-gen [a num]
    (let [n (max 1 (int num))]
      (loop [i 0
             a a]
        (if (>= i n)
          a
          (recur (inc i) (next-gen a))))))

  IEditable
  (cell-states [_]
    (:cell-states m))

  (cycle-cell [_ cell]
    (let [new-st ((:cycle-cell-fn m) (:state m) cell)]
      (-> m
          (update :history h/push new-st)
          (assoc :state new-st)
          Automata.)))

  IHistory
  (redo [_]
    (-> m
        (update :history h/redo)
        (as-> it (assoc it :state (h/head (:history it))))
        Automata.))

  (undo [_]
    (-> m
        (update :history h/undo)
        (as-> it (assoc it :state (h/head (:history it))))
        Automata.))

  (reset [_]
    (-> m
        (update :history h/reset)
        (update :history h/push (:init-st m))
        (as-> it (assoc it :state (h/head (:history it))))
        Automata.))

  (blank [_]
    (-> m
        (update :history h/reset)
        (update :history h/push (:blank-st m))
        (as-> it (assoc it :state (h/head (:history it))))
        Automata.))

  (total [_]
    (-> m :history h/total))

  (can-redo? [_]
    (-> m :history h/can-redo?))

  (can-undo? [_]
    (-> m :history h/can-undo?))

  (undo-levels [_]
    (-> m :history :keep))

  (set-undo-levels [_ n]
    (let [n (max 0 (int n))]
      (-> m
          (assoc-in [:history :keep] n)
          Automata.)))

  IDrawable
  (formats [_] (:formats m))

  (renderer [_] (:renderer-fn m))

  (renderer [a fmt] (renderer a)) ; FIXME

  (geometry [_] {:rows 100 :cols 100}) ; FIXME

  (state [_] (:state m)))

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
  [{:keys [name f init-st renderer-fn blank-st cycle-cell-fn cell-states undo-levels] :as args}]
  (let [m
        (cond-> args
          (nil? name)
          (assoc :name "Unknown automata")

          (nil? blank-st)
          (assoc :blank-st init-st)

          (or (nil? undo-levels) (< undo-levels 0))
          (assoc :undo-levels 0)

          (nil? cell-states)
          (assoc :cell-states [true false])

          (nil? cycle-cell-fn)
          (assoc :cycle-cell-fn identity)

          true
          (as-> it
              (assoc it :history (h/init undo-levels init-st))
              (assoc it :formats [:2d])
              (assoc it :state init-st)))]

    (Automata. m)))
