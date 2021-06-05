(ns cel-aut.automatas-test
  (:require
   [cel-aut.automatas :as aut]
   [cel-aut.model.automata :as ma]
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true :refer [deftest testing is]])))

(deftest conway-modify
  (testing "Modifications work in conway"
    (let [ca  (nth aut/automatas-model 0)
          ca  (ma/blank ca)
          cam (ma/cycle-cell ca 1)]
      (is (= false (ma/can-undo? ca)))
      (is (not= ca cam))
      (is (not (nil? (ma/state ca))))
      (is (= false (-> ca ma/state (get 1))))
      (is (= true (-> cam ma/state (get 1)))))))
