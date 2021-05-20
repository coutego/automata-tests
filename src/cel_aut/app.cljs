(ns cel-aut.app
  "Automata sample app"
  (:require
   [cel-aut.ui-automata :as ui-a]
   [cel-aut.automatas :as as]
   [reagent.dom :as d]))

(defn- automata [a]
  [:div
   [:h2 (:name a)]
   [ui-a/ui-automata (:f a) (:initial-state a)
    (into
      {:delay 0 :throttle 16 :keep 100}
      (when-let [d (:cell-renderer a)]
        {:cell-renderer d}))]])

(defn- separator [acc n]
  (-> acc
      (conj [:div.ui.divider {:style {:margin-top :3rem}}])
      (conj n)))

(defn home-page []
  [:<>
   [:div
    {:style {:max-width :600px :margin-left :3% :margin-right :3% :margin-bottom :2rem :align :center}}
    [:div.ui.container
     [:h1 "Cellular automata tests "
      [:a {:href "https://github.com/pedroabelleira/automata-tests"
           :target :blank
           :style {:color "hsl(40, 10%, 60%)"
                   :text-decoration :none}}
       [:i.ui.github.small.icon]]]
     (->> as/automatas
          (map automata)
          (reduce separator))
     [:div {:style {:margin-top :2rem :opacity "0%"}} " - "]]]])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
