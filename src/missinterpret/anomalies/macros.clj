(ns missinterpret.anomalies.macros
  (:require [clojure.spec.alpha :as s]
            [missinterpret.anomalies.anomaly :as anom]
            [missinterpret.anomalies.macros-spec :as spec]))

;; macro that adds support for argument
;; validation. Performs the following checks:
;;
;;  1. Wraps body with a try-catch
;;     a. if ex-data is an anomaly it is passed through
;;     b. If ex-data is not, a new anomaly is returned

;; https://www.codementor.io/@yehonathansharvit/how-to-write-custom-defn-macros-with-clojure-spec-supk887tw

(defn argument-valid? [a]
  (not (anom/anomaly? a)))

(defn first-error
  "Returns the first argument with an issue"
  [arguments]
  (loop [args arguments]
    (let [arg (first args)]
      (if (anom/anomaly? arg)
        arg
        (recur (rest args))))))

(defn unpack-args [args]
  (map
    #(last %)
    (get-in args [:args])))

(defn wrap-try [name args body]
  `((try
      (let [~'arguments (unpack-args ~args)]
        (cond
          (and (seq ~'arguments)
               (not (every? argument-valid? ~'arguments)))
          (first-error ~'arguments)

          :else
          (do
            ~@body)))
      (catch Exception ~'e
        (let [~'data (ex-data ~'e)
              ~'msg (.getMessage ~'e)]
          (cond
            (anom/anomaly? ~'data) ~'data

            :else
            (anom/anomaly
              (keyword (str *ns*) ~name)
              :anomaly.category/fault
              {:readable "Exception thrown"
               :data {:ex ~'e
                      :msg ~'msg
                      :arguments (unpack-args ~args)}})))))))

(defmacro let*
  [bindings body]
  (assert (vector? bindings) "a vector for its binding")
  (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
  #_(assert-args
    (vector? bindings) "a vector for its binding"
    (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [form (bindings 0) result (bindings 1)]
    `(let [temp# ~result]
       (if (anom/anomaly? temp#)
         temp#
         (let [~form temp#]
           ~@body)))))

#_(defmacro if-let*
  ([bindings & body]
   (if (seq bindings)
     ;; TODO: Figure out how to wrap the second binding with an anomaly check
     ;;       that returns nil
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))


;; Format of spec conform with changes from defn-spec
;;
;; Single Arity
;; {:name bar,
;;  :docstring "bar function",
;;  :meta {:private true},
;;  :bs [:arity-1
;;       {:args {},
;;        :body [:body [(+ 1 2)]]}]}
;;
;; Multiple Arity
;; {:name bar,
;;  :docstring "bar function",
;;  :meta {:private true},
;;  :bs [:arity-n
;;        {:bodies
;;          [{:args {},
;;            :body [:body [(+ 1 2)]]}
;;
;;        {:args {:args [[:sym a] [:sym b]]},
;;          :body [:body [(+ a b)]]}]}]}
;;
(defn update-body [{:keys [bs]:as conf} body-update-fn]
      (cond
        (= (first bs) :arity-1)
        (let [body-location [:bs 1 :body 1]
              args (get-in conf [:bs 1 :args])]
          (update-in conf body-location (partial body-update-fn args)))

        (= (first bs) :arity-n)
        (let [bodies (get-in conf [:bs 1 :bodies]),
              new-bodies
              (mapv (fn [body]
                      (let [body-location [:body 1]
                            args (:args body)]
                        (update-in body body-location (partial body-update-fn args))))
                    bodies)]
          (assoc-in conf [:bs 1 :bodies] new-bodies))))

;; NOTE: Any future macros that do not rely on this macro need to come *before*
(defmacro defn [& args]
  (let [{:keys [name] :as conf} (s/conform ::spec/defn-args args)
        new-conf (update-body conf (partial wrap-try (str name)))
        new-args (s/unform ::spec/defn-args new-conf)]
    (cons `clojure.core/defn new-args)))

