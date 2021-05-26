(ns cel-aut.dev
  (:require
   [reagent.core :as r]
   [malli.core :as m]
   [malli.generator :as mg]))

(defonce dev (r/atom false))
(defn in-dev? [] @dev)

(defn sum [a b] (+ a b))
(def =>sum [:=> [:cat int? int?] int?])
(time (m/validate =>sum sum  {::m/function-checker mg/function-checker}))
