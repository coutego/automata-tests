(ns cel-aut.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [clojure.core.async :as async :refer [put! <! >!]]))

(defn start [running? current-state-ref f >state]
  (async/go-loop []
    (when @running?
      (>! >state @current-state-ref)
      (<! (async/timeout 100))
      (swap! current-state-ref f)
      (recur))))

(defn create-model [board-ref f initial-state]
  (let [current  (atom initial-state)
        <command (async/chan 100)
        >state   (async/chan 100)
        running? (r/atom false)
        _        (put! >state @current)]
    (async/go-loop []
      (let [c (<! <command)]
        (case c
          :start (do
                    (reset! running? true)
                    (start running? current f >state))
          :stop  (reset! running? false)
          :reset (do
                    (reset! current initial-state)
                    (>! >state @current))
          (js/alert (str "Command not recognized " c))))
      (recur))
    [<command >state running?]))

(defn paint-cell [n val ctx]
  (let [row    (* 5 (quot n 100))
        column (* 5 (rem n 100))]
    (when val
      (.fillRect ctx row column 4 4))))
      ;(.strokeRect ctx row column 4 4))))

(defn paint [state board-ref]
  (let [ctx (.getContext @board-ref "2d")]
    (.clearRect ctx 0 0 500 500) ;(.width @board-ref) (.height @board-ref))
    (doall
     (map-indexed
      #(paint-cell %1 %2 ctx)
      state))))

(defn painter [<state board-ref]
  (async/go-loop []
    (let [new-state (<! <state)]
      (paint new-state board-ref))
    (recur)))

(defn ui-automata [f initial-state opts]
  (r/with-let [board-ref (r/atom nil)
               [>command <state running?]  (create-model board-ref f initial-state)
               _ (painter <state board-ref)
               button-style {:margin :0.5rem :padding :0.5rem}]
    [:<>
     [:div
      [:button {:style button-style :on-click #(put! >command (if @running? :stop :start))}
       (if @running? "Stop" "Start")]
      ;[:button {:style button-style :on-click #(put! >command :stop)} "Stop"]
      [:button {:style button-style :on-click #(put! >command :reset)} "Reset"]]
     [:div
      [:canvas
       {:width  500
        :height 500
        :ref    (fn [el] (reset! board-ref el))
        :style  {:background-color :#eee
                 :margin           :1rem
                 :border-color     :#000}}]]]))

(def initial-state-rand (mapv (fn [_] (< (rand-int 10) 2)) (range 10000)))
(def initial-state-e
  (-> (mapv (fn [_] false) (range 10000))
      (assoc 4849 1)
      (assoc 4850 1)
      (assoc 4851 1)
      (assoc 4949 1)
      (assoc 5049 1)
      (assoc 5050 1)
      (assoc 5051 1)
      (assoc 5149 1)
      (assoc 5249 1)
      (assoc 5250 1)
      (assoc 5251 1)))

(defn v [state x y]
  (let [x (mod x 100)
        y (mod y 100)]
    (get state (+ y (* 100 x)))))

(defn neighbourgs [n state]
  (let [x           (quot n 100)
        y           (rem n 100)]
    (+ (v state (dec x) (dec y))
       (v state (dec x) y)
       (v state (dec x) (inc y))
       (v state x (dec y))
       (v state x (inc y))
       (v state (inc x) (dec y))
       (v state (inc x) y)
       (v state (inc x) (inc y)))))

(defn conway-xy [n val state]
  (let [nei (neighbourgs n state)]
    (cond
      (and val (< nei 2))       false
      (and val (<= 2 nei 3))    true
      (and val (> nei 3))       false
      (and (not val) (= nei 3)) true
      :else                     false)))

(defn parity-xy [n state] (= 1 (rem (neighbourgs n state) 2)))

(defn conway [state]
  (into [] (map-indexed (fn [n v] (conway-xy n v state)) state)))

(defn parity [state]
  (into [] (map-indexed (fn [n _] (parity-xy n state)) state)))

(defn home-page []
  [:div
   [:h1 "Cellular automata test project"]
   [:h2 "Conway"]
   [ui-automata conway initial-state-rand]
   [:h2 "Parity"]
   [ui-automata parity initial-state-e]])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
