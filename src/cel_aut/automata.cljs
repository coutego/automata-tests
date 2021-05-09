(ns cel-aut.automata
  "Cellular automata reagent component"
  (:require
   [reagent.core :as r]
   [clojure.core.match :refer-macros [match]]
   [clojure.core.async :as async :refer [go-loop chan timeout put! <! >!]]))

(defn- next-action
  "Calculates the next state resulting from a :next action"
  [current f & [stop?]]
  (-> (if stop? (assoc current :running? false) current)
      (as-> it (update it :history conj (:board it)))
      (update :history (comp vec #(drop (- (count %) (:keep current)) %)))
      (update :board f)))

(defn- previous-action
  "Calculates the previous state resulting from a :next action"
  [current]
  (-> current
      (assoc :running? false)
      (update :history (comp vec butlast))
      (as-> it
          (if-let [prev (last (:history current))]
            (assoc it :board prev)
            it))))

(defn- launch-board-update-agent
  "Creates an 'agent' which keeps updating the state (by mutating the
  `current-state-ref`) for as long as `running?` refers to true. If
  exits as soon as `running?` refers to false"
  [current f >state]
  (swap! current assoc :running? true)
  (go-loop []
    (when (:running? @current)
      (>! >state (swap! current #(next-action % f)))
      (<! (timeout (max 1 (or (:delay @current) 200))))
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
  [f initial-board delay keep throttle]
  (let [current  (atom
                  {:board    initial-board
                   :running? false
                   :delay    delay
                   :keep     (max 0 (or keep 0))
                   :throttle (max 0 (or throttle 0))
                   :history  []})
        <command (chan 10)
        >state   (chan 10)]
    (put! >state @current)
    (go-loop []
      (when-let [c (<! <command)]
        (match c
          :start        (launch-board-update-agent current f >state)
          :stop         (put! >state (swap! current assoc :running? false))
          :reset        (put! >state (swap! current #(-> %
                                                         (assoc :running? false)
                                                         (assoc :board initial-board)
                                                         (assoc :history []))))
          :next         (put! >state (swap! current #(next-action % f true)))
          :previous     (put! >state (swap! current previous-action))
          {:delay x}    (put! >state (swap! current assoc :delay x))
          {:keep x}     (put! >state (swap! current assoc :keep x))
          {:throttle x} (put! >state (swap! current assoc :throttle x))
          :finish       (do (async/close! <command) (async/close! >state))
          (js/alert (str "Command not recognized " c)))
        (recur)))
    [<command >state]))

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

(defn- throttled-board-chan
  "From a `<state` channel, create a throttle channel where the boards are written.
  The throttling time is taken from the state"
  [<state]
  (let [tmp (chan (async/sliding-buffer 1))
        ret (chan)]
    (async/pipe <state tmp)
    (go-loop []
      (when-let [st (<! tmp)]
        (let [thr (-> st :throttle (or 0) (max 0) (min 5000))]
          (>! ret (:board st))
          (<! (timeout thr))
          (recur))))
    ret))

(defn- launch-painter-agent
  "Creates a new painter 'agent' that reads new states from the `<state`
  channel and redraws the canvas accordingly."
  [<state board-ref]
  (let [c (throttled-board-chan <state)]
    (go-loop []
      (when-let [new-state (<! c)]
        (paint new-state board-ref)
        (recur)))))

(defn- ui-input [label val on-click]
  [:span
   [:label label]
   [:input {:id :delay
            :type :text :value (str val)
            :style {:margin :0.5rem :padding :0.5rem}
            :size :8
            :on-change
            (fn [e] (try
                      (on-click (int (-> e .-target .-value)))
                      (catch :default e
                        (js/alert (str "Wrong value " (-> e .-target .-value))))))}]])

(defn ui-button [label on-click]
  [:button {:style {:margin :0.5rem :padding :0.5rem :width :6rem} :on-click on-click} label])

(defn ui-automata
  "Reagent component for an automata with the given initial board
  (a vector of 100 x 100 elements where the element with coordinates
  (x, y) is located at 100x + y) and the given transition function `f`.
  `f` is a function from one state to the next one.
  The supported options are:
  - `delay`: delay in ms between generations when in running mode
  - `throttle`: time in ms to use when throttling paints (i.e. paints will happen at most
    every that number of ms)
  - `keep`: number of generations to keep in memory as to be able to go to the previous board"
  [f initial-board {:keys [delay throttle keep]
                    :or {delay 200 throttle 0 keep 100}
                    :as opts}]
  (r/with-let [board-ref          (r/atom nil)
               state              (r/atom nil)
               [>command <state]  (launch-model-agent f
                                                      initial-board
                                                      (max 1 delay)
                                                      keep
                                                      throttle)
               [<st1 <st2]        (let [m (async/mult <state)
                                        ms1 (chan 2)
                                        ms2 (chan 2)]
                                    (async/tap m ms1)
                                    (async/tap m ms2)
                                    [ms1 ms2])
               _                    (launch-painter-agent <st1 board-ref)]
    (go-loop []
      (when-let [s (<! <st2)]
        (reset! state s)
        (recur)))
    [:<>
     [:div
      (if (:running? @state)
        [ui-button "Stop" #(put! >command :stop)]
        [ui-button "Start" #(put! >command :start)])
      [:<>
        [ui-button "Previous" #(put! >command :previous)]
        [ui-button "Next" #(put! >command :next)]
        [ui-button "Reset" #(put! >command :reset)]]
      [:div
       [ui-input "Delay" (:delay @state) #(put! >command {:delay %})]
       [ui-input "Throttle time" (:throttle @state) #(put! >command {:throttle %})]
       [ui-input "Undo levels" (:keep @state) #(put! >command {:keep %})]]]

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
