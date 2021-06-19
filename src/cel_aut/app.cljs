(ns cel-aut.app
  "Automata sample app"
  (:require
   [cel-aut.components.ui-automata :as ui-a]
   [cel-aut.automatas :as as]
   [cel-aut.model.automata :as am]
   [reagent.dom :as d]))

(defn- ui-automata-section [a]
  [:div
   [:h2 (am/aut-name a)]
   [ui-a/ui-automata a {:delay 50 :throttle 16}]])

(defn- separator-reducer [acc n]
  (-> acc
      (conj [:div.ui.divider {:style {:margin-top :3rem :margin-bottom :0.5rem}}])
      (conj n)))

(defn home-page []
  [:<>
   [:div
    {:style {:max-width :600px :margin-left :3% :margin-right :3%
             :margin-bottom :2rem :align :center}}
    [:div.ui.container
     [:h1 "Cellular automata tests "
      [:a {:href "https://github.com/pedroabelleira/automata-tests"
           :target :blank
           :style {:color "hsl(40, 10%, 60%)"
                   :text-decoration :none}}
       [:i.ui.github.small.icon]]]

     (->> as/automatas-model
          (map ui-automata-section)
          (reduce separator-reducer))

     [:div {:style {:margin-top :2rem :opacity "0%"}} " - "]]]])

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
