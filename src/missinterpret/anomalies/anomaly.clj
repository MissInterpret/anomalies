(ns missinterpret.anomalies.anomaly
  (:require  [clojure.spec.alpha :as s]
             [slingshot.slingshot :as sling]))

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
