(ns cel-aut.automatas-test
  (:require
   [cel-aut.automatas :as sut]
   [cel-aut.model.automata :as ma]
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true :refer [deftest testing is]])))

(deftest conway-array-modify
  (testing "Modifications work in conway-array"
    (let [ca  (nth sut/automatas-model 1)
          ca  (ma/blank ca)
          cam (ma/cycle-cell ca 1)]
      (is (= false (ma/can-undo? ca)))
      (is (not= ca cam))
      (is (not (nil? (:state ca))))
      (is (= false (-> ca :state (aget 1))))
      (is (= true (-> cam :state (aget 1)))))))

(deftest conway-modify
  (testing "Modifications work in regular conway"
    (let [ca  (nth sut/automatas-model 0)
          ca  (ma/blank ca)
          cam (ma/cycle-cell ca 1)]
      (is (= false (ma/can-undo? ca)))
      (is (not= ca cam))
      (is (not (nil? (:state ca))))
      (is (= false (-> ca :state (get 1))))
      (is (= true (-> cam :state (get 1)))))))
