(ns cel-aut.model.history-test
  (:require
   [cel-aut.model.history :as sut]
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true :refer [is deftest testing]])))

(deftest init
  (testing "Checks that history objects can be created"
    (let [h1 (sut/init 2 1 2 3 4 5)
          h2 (-> h1 sut/undo sut/undo)
          h3 (-> h2 sut/undo sut/undo)]
      (is (= (sut/head h1) 5))
      (is (= (sut/head h2) 3))
      (is (= (sut/head h3) 3))
      (is (= h3 h2)))))

(deftest redo
  (testing "Checks that changes can be redone"
    (let [h1 (sut/init 2 1 2 3 4 5)
          h2 (-> h1 sut/undo sut/undo sut/redo sut/redo)
          h3 (-> h2 sut/undo sut/undo)
          h4 (-> h2 sut/redo sut/redo sut/redo sut/redo)]
      (is (= (sut/head h2) 5))
      (is (= (sut/head h3) 3))
      (is (= (sut/head h4) 5)))))
