(ns cel-aut.automata
  "Cellular automata reagent component"
  (:require
   [reagent.core :as r]
   [clojure.core.match :refer-macros [match]]))

(defn- paint-cell [n val ctx cell-renderer]
  (let [x     (* 5 (quot n 100))
        y     (* 5 (rem n 100))
        color (cell-renderer val)]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx x y 4 4)))

(defn- paint [st]
  (when-let [ctx (some-> (:canvas st) (.getContext "2d"))]
    (.clearRect ctx 0 0 500 500)
    (let [cr (:cell-renderer st)
          b  (:board st)]
      (dorun (map-indexed (fn [n val] (paint-cell n val ctx cr)) b)))))

(defn do-next [st]
  (-> st
      (update :history conj (:board st))
      (update :history (comp vec #(drop (- (count %) (:keep st)) %)))
      (update :count inc)
      (update :board (:f st))))

(defn- do-previous [st]
  (-> st
      (assoc :running? false)
      (update :history (comp vec butlast))
      (as-> it
          (if-let [prev (last (:history it))]
            (-> it
                (assoc :board prev)
                (update :count dec))
            it))))

(defn- -do-start [st-ref]
  (js/setTimeout
   (fn []
     (when (:running? @st-ref)
       (swap! st-ref do-next)
       (-do-start st-ref)))
   (max 0 (or (:delay @st-ref) 0))))

(defn- do-start [st-ref]
  (swap! st-ref assoc :running? true)
  (-do-start st-ref))

(defn- do-click [st x y]
  (let [canvas (:canvas st)
        el-x   (+ (.-offsetLeft canvas) (.-clientLeft canvas))
        el-y   (+ (.-offsetTop canvas) (.-clientTop canvas))
        x      (quot (- x el-x) 5)
        y      (quot (- y el-y) 5)
        p      (+ (* 100 x) y)]
    (-> st
        (update-in [:board p] not))))

(defn command [st command]
  (match command
    {:start st-ref} (do-start st-ref)
    :stop           (assoc st :running? false)
    :reset          (assoc st
                           :board (:initial-board st)
                           :history []
                           :count 0
                           :running? false)
    :next           (do-next st)
    :previous       (do-previous st)
    {:delay x}      (assoc st :delay x)
    {:keep x}       (assoc st :keep x)
    {:throttle x}   (assoc st :throttle x)
    {:click [x y]}  (do-click st x y)
    :else           (do
                      (js/console.error "Command not implemented: " command)
                      st)))

(defn ui-button [label on-click & [inactive?]]
  [:button
   {:on-click on-click
    :style
    (into {:margin :0.5rem :padding :0.5rem :width :6rem}
          (when inactive? {:enabled :false}))}
   label])

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

(defn- make-renderer [throttle]
  (tap> {:make-renderer throttle})
  (goog.functions.throttle paint throttle))

(defn on-state-change [st o n]
  (when (or (not= (:board o) (:board n))
            (not= (:canvas o) (:canvas n)))
    (when-let [r (:renderer n)]
      (r n)))
  (when (not= (:throttle o) (:throttle n))
    (js/setTimeout
     #(swap! st assoc :renderer (make-renderer (:throttle n)))
     1)))

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
  [f initial-board {:keys [delay throttle keep cell-renderer]
                    :or   {delay 200 throttle 0 keep 100
                           cell-renderer (fn [x] (if x "#000" "#eaeaea"))}}]
  (r/with-let
    [throttle (max 0 (or throttle 0))
     state    (r/atom {:initial-board initial-board
                       :board         initial-board
                       :cell-renderer cell-renderer
                       :renderer      (make-renderer throttle)
                       :f             f
                       :running?      false
                       :count         0
                       :delay         (max 0 (or delay 0))
                       :keep          (max 0 (or keep 0))
                       :throttle      throttle
                       :history       []})
     _        (add-watch
               state
               ::board-watch
               (fn [_ _ o n] (on-state-change state o n)))]
    [:<>
     [:div
      (if (:running? @state)
        [ui-button "Stop" #(swap! state command :stop)]
        [ui-button "Start" #(do-start state)])
      [:<>
       [ui-button
        "Previous"
        #(swap! state command :previous) (= (count (:history @state)) 0)]
       [ui-button "Next" #(swap! state command :next)]
       [ui-button "Reset" #(swap! state command :reset)]]]
     [:div
      [ui-input "Delay"
       (:delay @state) #(swap! state command {:delay %})]
      [ui-input "Throttle time"
       (:throttle @state) #(swap! state command {:throttle %})]
      [ui-input "Undo levels"
       (:keep @state) #(swap! state command {:keep %})]]
     [:div
      {:style {:margin-bottom :0.5rem}}
      "Generations: " (:count @state)]

     [:div
      {
       :style  {:padding          "7px 6px 6px 7px"
                :border-radius    :0.3rem
                :border           "1px solid #eaeaea"
                :width  :500px
                :height :500px
                :background-color :#fff}}
      [:canvas
       {:on-mouse-up #(swap! state command {:click [(-> % .-pageX) (-> % .-pageY)]})
        :width  500
        :height 500
        :ref    (fn [el] (swap! state assoc :canvas el))
        :style  {:background-color :#fff}}]]]

    (finally (remove-watch state ::board-watch))))
