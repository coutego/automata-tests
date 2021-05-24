(ns cel-aut.components.ui-automata
  "Cellular automata reagent component"
  (:require
   [cel-aut.model.automata :as aut]
   [clojure.core.match :refer-macros [match]]
   [goog.async.nextTick]
   [goog.functions :as gf]
   [reagent.core :as r]))

(defn- paint-cell [cols n val render-fn ctx]
  (let [x     (* 10 (quot n cols))
        y     (* 10 (rem n cols))
        color (try (render-fn val) (catch :default e (println "Error: " e)))]
    (set! (.-fillStyle ctx) (or color "hsl(40, 10%, 10%)"))
    (.fillRect ctx x y 9 9)))

(defn- paint-board [st]
  (when-let [ctx (some-> (:canvas st) (.getContext "2d"))]
    (.clearRect ctx 0 0 1000 1000)
    (let [a         (:automata st)
          render-fn (aut/renderer a)
          cols      (-> a aut/geometry :cols)]
      (dorun (map-indexed (fn [n val] (paint-cell cols n val render-fn ctx))
                          (:state a))))))

(defn- -do-start [st-ref]
  (let [del (max 0 (or (:delay @st-ref) 0))
        f   (fn []
              (when (:running? @st-ref)
                (swap! st-ref (fn [s] (update s :automata aut/next-gen)))
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
        x      (quot (* ratio (- x el-x)) 10)
        y      (quot (* ratio (- y el-y)) 10)
        a      (:automata st)
        cols   (:cols (aut/geometry a))
        n      (+ y (* x cols))]
    (update st :automata #(aut/cycle-cell % n))))

(defn command [st cmd]
  (match cmd
         {:start st-ref} (do-start st-ref)
         :stop           (assoc st :running? false)
         :reset          (update st :automata aut/reset)
         :clear          (update st :automata aut/clear)
         :next           (update st :automata aut/next-gen)
         :undo           (update st :automata aut/undo)
         :redo           (update st :automata aut/redo)
         {:delay x}      (assoc st :delay x)
         {:keep x}       (update st :automata aut/set-undo-levels x)
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
    :style    {:background-color "hsl(40 20% 85%)"
               :cursor           (if inactive? :default :pointer)
               :color            (if inactive? "hsl(40 20% 75%)" "hsl(40 20% 25%)")}}
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
  (when (or (not= (:automata o) (:automata n))
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
  [a {:keys [delay throttle]
      :or   {delay 200 throttle 0}}]
  (let
      [throttle (max 0 (or throttle 0))
       state    (r/atom {:automata      a
                         :renderer      (make-renderer throttle)
                         :running?      false
                         :delay         (max 0 (or delay 0))
                         :throttle      throttle
                         :info-visible? false})
       _        (add-watch
                 state
                 ::automata-watch
                 (fn [_ _ o n] (on-state-change state o n)))
       clean-up (fn [] (remove-watch state ::automata-watch))]
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
        (or (:running? @state) (not (aut/can-undo? (:automata @state))))])

     :ui-redo-button
     (fn []
       [ui-button
        "redo"
        #(swap! state command :redo)
        (or (:running? @state) (not (aut/can-redo? (-> @state :automata))))])

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
        (-> @state :automata aut/undo-levels) #(swap! state command {:keep %})])

     :ui-generation-input
     (fn []
       [ui-input "Generation"
        (-> @state :automata aut/total) (fn []) true])

     :ui-board
     (fn []
       [:div
        {:style {:padding          "7px 7px 1px 8px"
                 :border-radius    :0.3rem
                 :border           "1px solid hsl(40 50% 90%)"
                 :background-color "hsl(40 20% 98.5%)"
                 :box-shadow       "0 0 10px hsl(40 10% 82%)"}}
        [:canvas
         {:on-mouse-up #(swap! state command {:click [(-> % .-pageX) (-> % .-pageY)]})
          :width       1000
          :height      1000
          :ref         (fn [el] (swap! state assoc :canvas el))
          :style       {:background-color "hsl(40 20% 95%)"
                        :width            :100%
                        :cursor           :crosshair}}]])

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
  [a {:keys [delay throttle cell-renderer]
      :or   {delay 200 throttle 0}
      :as opts}]
  (r/with-let
    [{:keys [ui-board ui-start-button ui-undo-button ui-redo-button ui-next-button
             ui-reset-button ui-clear-button info-visible? ui-delay-input
             ui-throttle-input ui-undo-input ui-generation-input ui-info-button
             clean-up]}
     (gen-ui-automata-components a opts)]

    [:<>
     [ui-board]
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
