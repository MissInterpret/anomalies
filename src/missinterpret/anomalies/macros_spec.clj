(ns missinterpret.anomalies.macros-spec
  (:require [clojure.core.specs.alpha]
            [clojure.spec.alpha :as spec]))

;; Specs that upgrade's clojure's ::defn-args spec to be inline
;; returning a vector ready for unform instead of a list
;;
;; Adapted from:
;; https://www.codementor.io/@yehonathansharvit/how-to-write-custom-defn-macros-with-clojure-spec-supk887tw
;; https://github.com/Engelberg/better-cond/blob/master/src/better_cond/core.cljc

(defn vec-unformer [a]
  (into []
        (mapcat (fn [x] (if (and (coll? x) (#{'& :as} (first x))) x [x])))
        a))

(spec/def ::arg-list
  (spec/and
    vector?
    (spec/conformer vec vec-unformer)
    (spec/cat :args (spec/* ::binding-form)
              :varargs (spec/? (spec/cat :amp #{'&} :form ::binding-form)))))

(spec/def ::args+body
  (spec/cat :args ::arg-list
            :body (spec/alt :prepost+body (spec/cat :prepost map?
                                                    :body (spec/+ any?))
                            :body (spec/* any?))))

(spec/def ::defn-args
  (spec/cat :name simple-symbol?
            :docstring (spec/? string?)
            :meta (spec/? map?)
            :bs (spec/alt :arity-1 ::args+body
                          :arity-n (spec/cat :bodies (spec/+ (spec/spec ::args+body))
                                             :attr (spec/? map?)))))

(spec/def ::binding-form
  (spec/or :sym :clojure.core.specs.alpha/local-name
           :seq ::seq-binding-form
           :map :clojure.core.specs.alpha/map-binding-form))

(spec/def ::seq-binding-form
  (spec/and
    vector?
    (spec/conformer vec vec-unformer)
    (spec/cat :elems (spec/* ::binding-form)
              :rest (spec/? (spec/cat :amp #{'&} :form ::binding-form))
              :as (spec/? (spec/cat :as #{:as} :sym :clojure.core.specs.alpha/local-name)))))

(spec/fdef defn
           :args ::defn-args
           :ret any?)

(spec/fdef defn-
           :args ::defn-args
           :ret any?)
