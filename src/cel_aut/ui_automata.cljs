(ns cel-aut.ui-automata
  "Cellular automata reagent component"
  (:require
   [goog.async.nextTick]
   [goog.functions :as gf]
   [reagent.core :as r]
   [cel-aut.history :as h]
   [clojure.core.match :refer-macros [match]]))

(defn- paint-cell [n val ctx cell-renderer]
  (let [x     (* 5 (quot n 100))
        y     (* 5 (rem n 100))
        color (cell-renderer val)]
    (set! (.-fillStyle ctx) (or color "hsl(40, 10%, 90%)"))
    (.fillRect ctx x y 4 4)))

(defn- paint-board [st]
  (when-let [ctx (some-> (:canvas st) (.getContext "2d"))]
    (.clearRect ctx 0 0 500 500)
    (let [cr (:cell-renderer st)
          b  (:board st)]
      (dorun (map-indexed (fn [n val] (paint-cell n val ctx cr)) b)))))

(defn do-next [st]
  (-> st
      (update :count inc)
      (update :board (:f st))
      (as-> it
        (update it :history h/push (:board it)))))

(defn- do-redo [st]
  (-> st
      (update :history h/redo)
      (as-> it (assoc it :board (h/head (:history it))))))

(defn- do-undo [st]
  (-> st
      (update :history h/undo)
      (as-> it (assoc it :board (h/head (:history it))))))

(defn- -do-start [st-ref]
  (let [del (max 0 (or (:delay @st-ref) 0))
        f   (fn []
              (when (:running? @st-ref)
                (swap! st-ref do-next true)
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
        ratio  (/ (.-width canvas) (.-clientWidth canvas))
        x      (quot (* ratio (- x el-x)) 5)
        y      (quot (* ratio (- y el-y)) 5)
        p      (+ (* 100 x) y)]
    (-> st
        (update-in [:board p] not)
        (as-> it
          (update it :history (fn [hi] (h/push hi (:board it))))))))

(defn command [st cmd]
  (match cmd
         {:start st-ref} (do-start st-ref)
         :stop           (assoc st :running? false)
         :reset          (assoc st
                                :board (:initial-board st)
                                :history (h/init (:initial-board st))
                                :count 0
                                :running? false)
         :clear          (-> st
                             (command :reset)
                             (assoc :board (mapv (constantly false) (range 10000)))
                             (as-> it (assoc it :history (h/init (:board it)))))
         :next           (do-next st)
         :undo           (do-undo st)
         :redo           (do-redo st)
         {:delay x}      (assoc st :delay x)
         {:keep x}       (assoc st :keep x)
         {:throttle x}   (assoc st :throttle x)
         {:click [x y]}  (do-click st x y)
         :toggle-info    (update st :info-visible? not)
         :else           (do
                           (js/console.error "Command not implemented: " cmd)
                           st)))

(defn ui-button [icon on-click & [inactive?]]
  [:div.ui.icon.button
   {:on-click (if inactive? #() on-click)
    :disabled (if inactive? :true :false)
    :style {:background-color "hsl(40 20% 85%)"
            :cursor (if inactive? :default :pointer)
            :color (if inactive? "hsl(40 20% 65%)" "hsl(40 20% 25%)")}}


   [:i.icon {:class icon}]])

(defn- ui-input [label val on-click & [disabled?]]
  [:div.field
   [:label label]
   [:input
    (into
     {:type :text :value (str val)
      :style {:padding :0.3rem}
      :size :6
      :on-change
      (fn [e] (try
                (on-click (int (-> e .-target .-value)))
                (catch :default e
                  (js/alert (str "Wrong value " (-> e .-target .-value))))))}
     (when disabled? {:disabled true}))]])

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
                    :or   {delay         200 throttle 0 keep 100
                           cell-renderer (fn [x] (if x "hsl(40, 50%, 20%)" "hsl(40, 10%, 90%)"))}}]
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
                         :history       (h/init initial-board)
                         :info-visible? false})
       _        (add-watch
                 state
                 ::board-watch
                 (fn [_ _ o n] (on-state-change state o n)))
       clean-up (fn [] (remove-watch state ::board-watch))]
    {:ui-start-button
     (fn []
       (if (:running? @state)
         [ui-button "pause circle" #(swap! state command :stop)]
         [ui-button "play circle" #(do-start state)]))

     :ui-undo-button
     (fn []
       [ui-button
        "undo"
        #(swap! state command :undo)
        (:running? @state)])

     :ui-redo-button
     (fn []
       [ui-button
        "redo"
        #(swap! state command :redo)
        (:running? @state)])

     :ui-next-button
     (fn []
       [ui-button "step forward" #(swap! state command :next) (:running? @state)])

     :ui-reset-button
     (fn []
       [ui-button "recycle" #(swap! state command :reset) (:running? @state)])

     :ui-clear-button
     (fn []
       [ui-button "trash" #(swap! state command :clear) (:running? @state)])

     :ui-info-button
     (fn []
       [ui-button "info circle" #(swap! state command :toggle-info)])

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

     :ui-generation-input
     (fn []
       [ui-input "Generation"
        (h/pos (:history @state)) (fn []) true])

     :ui-board
     [:div
      {
       :style {:padding          "7px 6px 1px 7px"
               :border-radius    :0.3rem
               :border           "1px solid hsl(40 50% 90%)"
               :background-color "hsl(40 20% 98.5%)"
               :box-shadow       "0 0 10px hsl(40 10% 82%)"}}
      [:canvas
       {:on-mouse-up #(swap! state command {:click [(-> % .-pageX) (-> % .-pageY)]})
        :width       500
        :height      500
        :ref         (fn [el] (swap! state assoc :canvas el))
        :style       {:background-color "hsl(40 20% 95%)"
                      :width            :100%}}]]

     :running?      (fn [] (:running? @state))
     :generation    (fn [] (:count @state))
     :info-visible? (fn [] (:info-visible? @state))
     :clean-up      clean-up}))

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
    [{:keys [ui-board ui-start-button ui-undo-button ui-redo-button ui-next-button
             ui-reset-button ui-clear-button info-visible? ui-delay-input
             ui-throttle-input ui-undo-input ui-generation-input ui-info-button
             clean-up]}
     (gen-ui-automata-components f initial-board opts)]

    [:<>
     ui-board
     [:div {:style {:margin :1rem}}]
     [:div.ui.buttons
      [ui-start-button] [ui-next-button] [ui-undo-button] [ui-redo-button]
      [ui-reset-button] [ui-clear-button]]
     [:span {:style {:margin-left :0.5rem}}]
     [ui-info-button]
     (when (info-visible?)
       [:div.ui.segment {:style {:background-color "hsl(40, 20%, 95%)"}}
        [:div.ui.form
         [:div.fields
          [ui-delay-input] [ui-throttle-input] [ui-undo-input] [ui-generation-input]]]])
     [:div {:style {:margin :0.4rem}}]]
    (finally (clean-up))))
