(ns missinterpret.anomalies.anomaly-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [missinterpret.anomalies.anomaly :as anom]
            [missinterpret.anomalies.anomaly-spec]))

;; copyright (c) 2024 Creative Commons BY 4.0 Deed


(def anomaly (gen/generate (s/gen :anomalies/anomaly)))

(deftest anomaly-create
  (is (s/valid? :anomalies/anomaly anomaly)))

(deftest validate
  (is (some? (anom/validate :anomalies/anomaly anomaly))))

(deftest anomly-cstrs
  (testing "No message"
    (is (anom/anomaly? (anom/anomaly ::test1 :anomaly.category/unavailable))))
  (testing "With message"
    (is (anom/anomaly? (anom/anomaly ::test2 :anomaly.category/unavailable {:test :A})))))

(deftest contains-anomaly
  (testing "True"
    (is (not (anom/contains? {}))))
  (testing "False"
    (is (anom/contains?
          (assoc (anom/anomaly ::here :anomaly.category/unavailable) :test :test)))))

(deftest anomaly-throw+
  (is (thrown?  clojure.lang.ExceptionInfo
                (anom/throw+
                  ::test
                  {:from ::here
                   :category :anomaly.category/fault
                   :message  "test"}))))

(deftest throw-if-cognitect-anomaly
  (is (thrown?
        java.lang.Exception
        (-> :cognitect.anomalies/anomaly
            s/gen
            gen/generate
            anom/throw-if-cognitect-anomaly))))

(deftest wrap-exception
  (testing "ex-data is anomaly"
    (let [data (try
                 (anom/throw+
                   :invalid-test
                   {:from     ::test
                    :category :anomaly.category/invalid
                    :message  {}})
                 (catch Exception ex (anom/wrap-exception ex)))]
      (is (anom/anomaly? data))))
  (testing "ex-data not anomaly; Passed arg is anomaly"
    (let [data (try
                 (throw (Exception. "my exception message"))
                 (catch Exception ex (anom/wrap-exception ex (anom/anomaly ::here :anomaly.category/invalid))))]
      (is (anom/anomaly? data))))
  (testing "Exception only"
    (let [data (try
                 (throw (Exception. "my exception message"))
                 (catch Exception ex (anom/wrap-exception ex)))]
      (is (anom/anomaly? data)))))


