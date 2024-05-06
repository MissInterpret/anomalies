(ns missinterpret.anomalies.anomaly
  (:require [clojure.spec.alpha :as s]
            [missinterpret.anomalies.anomaly-spec]
            [slingshot.slingshot :as sling]
            [cognitect.anomalies])
  (:import (java.time Instant)))

;; copyright (c) 2024 Creative Commons BY 4.0 Deed

;; Duplicate of categories from Cognitect Anomalies
(defonce categories #{:anomaly.category/unavailable
                      :anomaly.category/interrupted
                      :anomaly.category/incorrect
                      :anomaly.category/forbidden
                      :anomaly.category/unsupported
                      :anomaly.category/not-found
                      :anomaly.category/conflict
                      :anomaly.category/fault
                      :anomaly.category/busy})


(defn anomaly
  ([{:keys [from category message]}]
   (anomaly from category message))
  ([from category]
   (anomaly from category nil))
  ([from category message]
   (cond-> {:anomaly/from     from
            :anomaly/category category
            :anomaly/when     (Instant/now)}
           (some? message) (assoc :anomaly/message message))))


(defn anomaly? [m]
  (s/valid? :anomalies/anomaly m))


(defn contains? [m]
  (anomaly? (select-keys m [:anomaly/category
                            :anomaly/from
                            :anomaly/when
                            :anomaly/message])))


(defn throw+
  "Throws a slingshot Exception with a given type
   and an associated ex-info package that includes a
   map which conforms to the anomaly specification."
  [{:keys [from category message] :as arg}]
  (let [a (if (anomaly? arg)
            (-> arg (assoc :type :anomaly))
            (->  (anomaly from category message)
                 (assoc :type :anomaly)))]
    (sling/throw+ a)))


(defn throw-if-cognitect-anomaly
  "If this conforms to a cognitect anomaly wrap and throw it, otherwise
   it is passed through."
  [x]
  (when (s/valid? :cognitect.anomalies/anomaly x)
    (let [cat (->> (:cognitect.anomalies/category x)
                   name
                   (keyword "anomaly.category"))]
      (throw+
        {:from     ::throw-if-cognitect-anomaly
         :category cat
         :message  {:readable (str "Cognitect anomaly: " (:cognitect.anomalies/category x))
                    :reasons [:cognitect-anomaly]
                    :anomaly x}})))
  x)


(defn validate
  "Validates that the data conforms to the given spec throwing
   an anomaly if it does not, returning the data if it does."
  [spec data]
  (if (s/valid? spec data)
    data
    (throw+ {:from     ::validate
             :category :anomaly.category/incorrect
             :message  {:readable (str "The data does not conform to the spec: " (s/explain spec data))
                        :reasons  [:spec/invalid]
                        :data     {:spec spec :validating data}}})))


(defn wrap-exception
  ([ex]
   "Transforms an Exception (with or without ex-data) into an anomaly using:
     1. ex-info that contains the keys for an anomaly.
     2. A passed anomaly
     3. Constructs an anomaly out of the exception
        (if second arg is a keyword it is used as the `:from` in generated anomaly)"
   (wrap-exception ex nil))
  ([ex anomaly]
   (let [ex-data (ex-data ex)]
     (cond
       (contains? ex-data)
       (select-keys ex-data [:anomaly/category
                             :anomaly/from
                             :anomaly/when
                             :anomaly/message])

       (anomaly? anomaly) anomaly

       :else
       {:anomaly/category :anomaly.category/fault
        :anomaly/from     (if (keyword? anomaly) anomaly ::exception)
        :anomaly/when     (Instant/now)
        :anomaly/message  {:readable (str "Unhandled exception: " (.getMessage ex))
                           :reasons  [:exception]
                           :data     {:exception ex}}}))))

