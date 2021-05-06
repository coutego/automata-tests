(ns cel-aut.automata
  (:require
   [reagent.core :as r]
   [clojure.core.async
    :as async
    :refer [go-loop chan timeout put! <! >! close!]]))

(defn- launch-board-update-agent
  "Creates an 'agent' which keeps updating the state (by mutating the
  `current-state-ref`) for as long as `running?` refers to true. If
  exits as soon as `running?` refers to false"
  [running? >state next-state! delay]
  (go-loop []
    (when @running?
      (>! >state (next-state!))
      (<! (timeout (max 1 (or @delay 200))))
      (recur))))

(defn- launch-model-agent
  "Creates an 'agent' which represents the logical model of one automata
  (board + controls).
  It returns a triple [`>command` `<state` `running?`] where:
  - >command is a channel where the owner can send commands to this agent.
    Supported commands are :start, :stop, :reset and :finish.
    This channel is meant to only be written to.
  - <state is a channel where the owner can receive new states of the board,
    as they are generated. This channel is meant to only be read from.
  - running? is a function that returns true is the model is running and false
    otherwise."
  [f initial-state delay]
  (let [current         (atom initial-state)
        previous        (atom [])
        <command        (chan 100)
        >state          (chan 100)
        running?        (r/atom false)
        push-state!     (fn [st]
                          (swap! previous
                                 (fn [p]
                                   (let [c (count p)
                                         p (if (> c 10000)
                                             (vec (drop (- 10000 c) p))
                                             p)]
                                     (conj p @current))))
                          (reset! current st))
        push&put-state! (fn [st]
                          (push-state! st)
                          (put! >state @current))
        push-next-state! (fn [] (push-state! (f @current)))]
    (put! >state @current)
    (go-loop []
      (when-let [c (<! <command)]
        (case c
          :start    (do
                      (reset! running? true)
                      (launch-board-update-agent running?
                                                 >state
                                                 push-next-state!
                                                 delay))
          :stop     (reset! running? false)
          :reset    (do
                      (reset! running? false)
                      (push&put-state! initial-state))
          :next     (do
                      (reset! running? false)
                      (push&put-state! (f @current)))
          :previous (do
                      (reset! running? false)
                      (reset! current (or (last @previous) initial-state))
                      (swap! previous (comp vec butlast))
                      (put! >state @current))
          :finish   (do (close! <command)
                        (close! >state))
          (js/alert (str "Command not recognized " c)))
        (recur)))
    [<command >state (fn [] @running?)]))


(defn- paint-cell
  "Paints the cell of coordinates given by n in the given canvas context with
  value `val` (true or false)"
  [n val ctx]
  (let [row    (* 5 (quot n 100))
        column (* 5 (rem n 100))]
    (if val
      (set! (.-fillStyle ctx) "#000")
      (set! (.-fillStyle ctx) "#eaeaea"))
    (.fillRect ctx row column 4 4)))

(defn- paint
  "Paints the board of the given state on the given board reference"
  [state board-ref]
  (let [ctx (.getContext @board-ref "2d")]
    (.clearRect ctx 0 0 500 500)
    (doall
     (map-indexed
      #(paint-cell %1 %2 ctx)
      state))))

(defn- launch-painter-agent
  "Creates a new painter 'agent' that reads new states from the `<state`
  channel and redraws the canvas accordingly."
  [<state board-ref max-refresh]
  (go-loop [last-redraw 0]
    (when-let [new-state (<! <state)]
      (let [ct (.getTime (js/Date.))
            diff (- ct last-redraw)]
        (if (>= diff max-refresh)
          (do
            (paint new-state board-ref)
            (recur ct))
          (recur last-redraw))))))

(defn ui-automata
  "Reagent component for an automata with the given initial state
  (a vector of 100 x 100 elements where the element with coordinates
  (x, y) is located at 100x + y) and the given transition function `f`.
  `f` is a function from one state to the next one.
  The only supported option is `delay`, which is the number of miliseconds
  to wait before creating the new state."
  [f initial-state {:keys [delay max-refresh]
                    :or {delay 200 max-refresh 0}
                    :as opts}]
  (r/with-let [board-ref                   (r/atom nil)
               delay                       (r/atom (max 1 delay))
               [>command <state running?]  (launch-model-agent f
                                                               initial-state
                                                               delay)
               _                           (launch-painter-agent <state board-ref
                                                                 max-refresh)
               button-style                {:margin :0.5rem :padding :0.5rem}]
    [:<>
     [:div
      [:button {:style button-style :on-click #(put! >command
                                                     (if (running?) :stop :start))}
       (if (running?) "Stop" "Start")]
      [:button {:style button-style :on-click #(put! >command :previous)} "Previous"]
      [:button {:style button-style :on-click #(put! >command :next)} "Next"]
      [:button {:style button-style :on-click #(put! >command :reset)} "Reset"]
      [:span {:style {:margin-left :2rem}}]
      [:label {:for :delay} "Delay"]
      [:input {:id :delay
               :type :text :value (str @delay)
               :style button-style
               :size :8
               :on-change
               (fn [e] (try
                         (reset! delay (int (-> e .-target .-value)))
                         (catch :default e
                           (println e)
                           (js/alert "Exception"))))}]]
     [:div
      [:canvas
       {:width  500
        :height 500
        :ref    (fn [el] (reset! board-ref el))
        :style  {:padding          "7px 6px 6px 7px"
                 :border-radius    :0.3rem
                 :border           "1px solid #eaeaea"
                 :background-color :#fff}}]]]
    (finally
      (put! >command :finish))))
