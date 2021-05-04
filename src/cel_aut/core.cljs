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
        running? (atom false)]
    (async/go-loop []
      (let [c (<! <command)]
        (case c
          :start  (do
                   (reset! running? true)
                   (start running? current f >state))
          :stop   (reset! running? false)
          :reset  (do
                    (reset! current initial-state)
                    (>! >state @current))
          (>! >state (str "Command not recognized " c))))
      (recur))
    [<command >state]))

(defn paint-cell [n val ctx]
  (let [row    (mod n 100)
        column (rem n 100)]
    (if val
      (.fillRect row column 4 4)
      (.strokeRect row column 4 4))))

(defn paint [state board-ref]
  (println "board-ref: " @board-ref)
  (let [ctx (.getContext @board-ref "2d")]
    (.clearRect ctx 0 0 500 500) ;(.width @board-ref) (.height @board-ref))
    ;(.fillRect ctx 10 10 10 10)
    ;; (map-indexed
    ;;  #(paint-cell %1 %2 ctx)
    ;;  state)
    (set! (.-font ctx) "20px Arial")
    (.fillText ctx (str "State: " state) 10 30)))

(defn painter [<state board-ref]
  (async/go-loop []
    (let [new-state (<! <state)]
      (paint new-state board-ref))
    (recur)))

(defn ui-automata [f initial-state opts]
  (r/with-let [board-ref (r/atom nil)
               [>command <state]  (create-model board-ref f initial-state)
               _ (painter <state board-ref)
               button-style {:margin :0.5rem :padding :0.5rem}]
    [:<>
     [:div
      [:button {:style button-style :on-click #(put! >command :start)} "Start"]
      [:button {:style button-style :on-click #(put! >command :stop)} "Stop"]
      [:button {:style button-style :on-click #(put! >command :reset)} "Reset"]]
     [:div
      [:canvas
       {:width  500
        :height 500
        :ref    (fn [el] (reset! board-ref el))
        :style  {:background-color :#eee
                 :margin           :1rem
                 :border-color     :#000}}]]]))

(def initial-state (mapv (fn [n] (< (rand-int 10) 2)) (range 10000)))

(defn home-page []
 [:div [:h2 "Cellular automata test project"]
  [ui-automata inc 1]])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
