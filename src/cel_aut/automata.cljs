(ns cel-aut.automata
  "Cellular automata reagent component"
  (:require
   [goog.async.nextTick]
   [goog.functions :as gf]
   [reagent.core :as r]
   [clojure.core.match :refer-macros [match]]))

(defn- paint-cell [n val ctx cell-renderer]
  (let [x     (* 5 (quot n 100))
        y     (* 5 (rem n 100))
        color (cell-renderer val)]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx x y 4 4)))

(defn- paint-board [st]
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
          (if-let [prev (last (:history st))]
            (-> it
                (assoc :board prev)
                (update :count dec))
            it))))

(defn- -do-start [st-ref]
  (let [del (max 0 (or (:delay @st-ref) 0))
        f   (fn []
              (when (:running? @st-ref)
                (swap! st-ref do-next)
                (-do-start st-ref)))]
    (if (> del 0)
      (js/setTimeout f del)
      (this-as th (goog.async.nextTick f th)))))

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
    (into {:margin "0.5rem 1rem 0.5rem 0rem" :padding :0.5rem :width :6rem}
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
  (if (> throttle 0)
    (gf/throttle paint-board throttle)
    paint-board))

(defn on-state-change [st o n]
  (when (or (not= (:board o) (:board n))
            (not= (:canvas o) (:canvas n)))
    (when-let [r (:renderer n)]
      (r n)))
  (when (not= (:throttle o) (:throttle n))
    (js/setTimeout
     #(swap! st assoc :renderer (make-renderer (:throttle n)))
     1)))

(defn gen-ui-automata-components
  "Function (not reagent component) that creates the model + individual components
  for the automata. Clients can arrange and use the returned components and properties
  as they wish. Clients should call the :clean-up function on a r/with-let finally clause"
  [f initial-board {:keys [delay throttle keep cell-renderer]
                    :or   {delay 200 throttle 0 keep 100
                           cell-renderer (fn [x] (if x "#320" "#efeae9"))}}]
  (let
    [throttle (max 0 (or throttle 0))
     state    (r/atom {:initial-board initial-board
                       :f             f
                       :board         initial-board
                       :cell-renderer cell-renderer
                       :renderer      (make-renderer throttle)
                       :running?      false
                       :count         0
                       :delay         (max 0 (or delay 0))
                       :keep          (max 0 (or keep 0))
                       :throttle      throttle
                       :history       []})

     clean-up (fn [] (remove-watch state ::board-watch))
     _        (add-watch
               state
               ::board-watch
               (fn [_ _ o n] (on-state-change state o n)))]
    {:ui-start-button
     (fn []
       (if (:running? @state)
         [ui-button "Stop" #(swap! state command :stop)]
         [ui-button "Start" #(do-start state)]))

     :ui-prev-button
     (fn []
       [ui-button
        "Previous"
        #(swap! state command :previous) (= (count (:history @state)) 0)])

     :ui-next-button
     (fn []
       [ui-button "Next" #(swap! state command :next)])

     :ui-reset-button
     (fn []
       [ui-button "Reset" #(swap! state command :reset)])

     :ui-delay-input
     (fn []
       [ui-input "Delay"
        (:delay @state) #(swap! state command {:delay %})])

     :ui-throttle-input
     (fn []
       [ui-input "Throttle time"
        (:throttle @state) #(swap! state command {:throttle %})])

     :ui-undo-input
     (fn []
       [ui-input "Undo levels"
        (:keep @state) #(swap! state command {:keep %})])

     :ui-board
     [:div
      {
       :style  {:padding          "7px 6px 6px 7px"
                :border-radius    :0.3rem
                :border           "1px solid hsl(40 50% 90%)"
                :width  :500px
                :height :500px
                :background-color "hsl(40 20% 95%)"
                :box-shadow "0 0 10px hsl(40 10% 82%)"}}
      [:canvas
       {:on-mouse-up #(swap! state command {:click [(-> % .-pageX) (-> % .-pageY)]})
        :width  500
        :height 500
        :ref    (fn [el] (swap! state assoc :canvas el))
        :style  {:background-color "hsl(40 20% 95%)"}}]]

     :running? (fn [] (:running? @state))
     :generation (fn [] (:count @state))
     :clean-up clean-up}))

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
                           cell-renderer (fn [x] (if x "#420" "#faeaea"))}
                    :as opts}]
  (r/with-let
    [{:keys [ui-board ui-start-button ui-prev-button ui-next-button ui-reset-button
             generation running? ui-delay-input ui-throttle-input ui-undo-input clean-up]}
     (gen-ui-automata-components f initial-board opts)]

    [:<>
     [:div [ui-start-button] [ui-prev-button] [ui-next-button] [ui-reset-button]]
     [:div [ui-delay-input] [ui-throttle-input] [ui-undo-input]]
     [:div "Running: " (if (running?) "Yes" "No ")
      [:span {:style {:margin-left :2rem}}]
      "Generation: " [generation]]
     [:div {:style {:margin :1rem}}]
     ui-board]
    (finally (clean-up))))
