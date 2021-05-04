(ns cel-aut.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]
      [clojure.core.async :as async :refer [put! <! >!]]))

(defn create-model [board-ref]
  (let [command  (async/chan 100)
        state    (async/chan 100)
        running? (atom false)]
    (async/go-loop []
      (let [c (<! command)]
        (case c
         :start (>! state "Start!")
         :stop  (>! state "Stop!")
         (>! state (str "Command not recognized " c))))
      (recur))
    [command state]))

(defn paint [state board-ref]
  (println "board-ref: " @board-ref)
  (let [ctx (.getContext @board-ref "2d")]
    (.clearRect ctx 0 0 500 500) ;(.width @board-ref) (.height @board-ref))
    ;(.fillRect ctx 10 10 10 10)
    (set! (.-font ctx) "10px Arial")
    (.fillText ctx (str "Paint " state) 10 30)))

(defn painter [<state board-ref]
  (async/go-loop []
    (let [new-state (<! <state)]
      (paint new-state board-ref))
    (recur)))

(defn ui-automata [f st opts]
  (r/with-let [board-ref (r/atom nil)
               [>command <state]  (create-model board-ref)
               _ (painter <state board-ref)]
    [:<>
     [:div
      [:button {:on-click #(put! >command :start)} "Start"]
      [:button {:on-click #(put! >command :stop)} "Stop"]
      [:button {:on-click #(put! >command :finish)} "Finish"]]
     [:div
      [:canvas
       {:width  500
        :height 500
        :ref (fn [el] (reset! board-ref el))
        :style {:background-color :#eee}}]]]))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to Reagent!"]
   [ui-automata]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
