(ns missinterpret.anomalies.anomaly
  (:require [clojure.spec.alpha :as s]
            [missinterpret.anomalies.anomaly-spec]
            [slingshot.slingshot :as sling]
            [cognitect.anomalies])
  (:import (java.time Instant)))

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
  "Throws a slingshot exeception with a given type
   and an associated ex package that includes a
   map that conforms to the flow anomalies spec."
  [type {:keys [from category message]}]
  (sling/throw+
    (->  (anomaly from category message)
         (assoc :type type))))


(defn throw-if-cognitect-anomaly
  "If this conforms to a cognitect anomaly wrap and throw it, otherwise
   pass it through."
  [x]
  (when (s/valid? :cognitect.anomalies/anomaly x)
    (let [cat (->> (:cognitect.anomalies/category x)
                   name
                   (keyword "anomaly.category"))]
      (throw+
        :cognitect-anomaly
        {:from ::throw-if-cognitect-anomaly
         :category cat
         :message {:readable (str "Cognitect anomaly: " (:cognitect.anomalies/category x))
                   :reason :cognitect-anomaly
                   :anomaly x}})))
  x)


(defn validate
  "Validates that the data conforms to the given spec throwing
   an anomaly if it does not, returning the data if it does."
  [spec data]
  (if (s/valid? spec data)
    data
    (throw+ :invalid-spec {:from     ::validate
                           :category :anomaly.category/incorrect
                           :message  {:readable (str "The data does not conform to the spec: " (s/explain spec data))
                                      :reason :spec-failure
                                      :data {:spec spec
                                             :validating data}}})))


(defn wrap-exception
  ([ex]
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
          :anomaly/from     ::exception
          :anomaly/when     (Instant/now)
          :anomaly/message  {:readable (str "Unhandled exception: " (.getMessage ex))
                             :reason :anomaly/exception
                             :data  {:ex ex}}}))))

