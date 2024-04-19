(ns missinterpret.anomalies.macros-test
  (:require [clojure.test :refer :all]
            [missinterpret.anomalies.macros :refer [defn]]
            [missinterpret.anomalies.anomaly :as anom]))

(defn throw-test-fn [_]
   (throw (Exception. "Test Exception")))

(defn pass-test-fn [a]
  a)

(deftest macro-wrap
  (testing "Pass-through fn returns argument"
    (let [arg {:a :A}
          result (pass-test-fn arg)]
      (is (map? result))
      (is (contains? result :a))))
  (testing "Throw test fn returns an anomaly"
    (let [result (throw-test-fn nil)]
      (is (anom/anomaly? result)))))