(ns missinterpret.anomalies.macros-test
  (:require [clojure.test :refer :all]
            [missinterpret.anomalies.macros :refer [defn] :as a]
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

(deftest if-let+
  (testing "Successful binding, form evaluated"
    (let [result (a/if-let+ [b1 :a] b1)]
      (is (= :a result))))
  (testing "Anomaly in binding, returned"
    (let [a (anom/anomaly {:from ::repl :category :anomaly.category/fault :message "test"})
          result (a/if-let+ [b1 a] :a)]
      (is (= result a)))))

(deftest let+
  (testing "Successful bindings, form evaluated"
    (let [result (a/let+ [b1 :a b2 :b b3 :c]
                    [b1 b2 b3])]
      (is (= result [:a :b :c]))))
  (testing "Anomaly in bindings, returned"
    (let [a (anom/anomaly {:from ::repl :category :anomaly.category/fault :message "test"})
          result (a/let+ [b1 :a b2 a b3 :c])]
      (is (= result a)))))