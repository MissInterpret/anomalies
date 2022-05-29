(ns missinterpret.anomalies.anomaly-spec
    (:require [clojure.spec.alpha :as s]))

(s/def :anomaly/from     keyword?)
(s/def :anomaly/category keyword?)
(s/def :anomaly/message  some?)

(s/def :anomalies/anomaly (s/keys :req [:anomaly/category
                                        :anomaly/from]
                                  :opt [:anomaly/message]))
