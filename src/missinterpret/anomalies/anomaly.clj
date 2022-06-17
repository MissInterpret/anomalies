(ns missinterpret.anomalies.anomaly
  (:require  [clojure.spec.alpha :as s]
             [slingshot.slingshot :as sling]
             [cognitect.anomalies]))

(defonce categories #{:anomaly.category/unavailable
                      :anomaly.category/interrupted
                      :anomaly.category/incorrect
                      :anomaly.category/forbidden
                      :anomaly.category/unsupported
                      :anomaly.category/not-found
                      :anomaly.category/conflict
                      :anomaly.category/fault
                      :anomaly.category/busy})

(defn throw+
  "Throws a slingshot exeception with a given type
   and an associated ex package that includes a
   map that conforms to the flow anomalies spec."
  [type {:keys [from category message]}]
  (sling/throw+ {:type type
                 :anomaly/category category
                 :anomaly/from     from
                 :amnomly/when     (java.time.Instant/now)
                 :anomaly/message  message}))

(defn validate
  "Validates that the data conforms to the given spec throwing
   an anomaly if it does not, returning the data if it does."
  [spec data]
  (if (s/valid? spec data)
    data
    (throw+ :invalid-spec {:from     ::validate
                           :category :anomaly.category/incorrect
                           :message  {:readable (str "The data does not conform to the spec: " (s/explain spec data))
                                      :data {:spec spec
                                             :validating data}}})))



(defn throw-if-cognitect-anomaly
  "If this conforms to a cognitect anomaly wrap and throw it, otherwise
   pass it through."
  [i]
  (when (s/valid? :cognitect.anomalies/anomaly i)
    (let [cat (->> (:cognitect.anomalies/category i)
                  name
                  (keyword "anomaly.category"))]
      (throw+
        :cognitect-anomaly
        {:from ::throw-if-cognitect-anomaly
         :category cat
         :message {:readable (str "Cognitect anomaly: " (:cognitect.anomalies/category i))
                   :anomaly i}})))
  i)
