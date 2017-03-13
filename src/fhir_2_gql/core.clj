(ns fhir-2-gql.core
  (:require [clojure.data.json :refer [read-str]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn cardinality [element]
  (match [(:min element) (:max element)]
    [0 "1"] :optional
    [1 "1"] :one
    [0 "0"] :none
    :else :many))

(defn- upper-case? [str]
  (Character/isUpperCase (first str)))

(derive ::dateTime ::instant)

(derive ::base64Binary ::string)
(derive ::code ::string)
(derive ::markdown ::string)
(derive ::uri ::string)
(derive ::oid ::string)

(derive ::unsignedInt ::integer)
(derive ::positiveInt ::integer)

(defmulti type-name (comp #(keyword "fhir-2-gql.core" %) :code first))
(defmethod type-name ::instant [_] "Instant") ;; TODO define Instant scalar type in overall gql schema
(defmethod type-name ::time [_] "Time") ;; TODO define Time scalar type in overall gql schema
(defmethod type-name ::date [_] "Date") ;; TODO define Date scalar type in overall gql schema
(defmethod type-name ::boolean [_] "Boolean")
(defmethod type-name ::id [_] "ID")
(defmethod type-name ::decimal [_] "Float")
(defmethod type-name ::string [_] "String")
(defmethod type-name ::integer [_] "Int")
(defmethod type-name :default [v]
  (let [base-type (:code (first v))]
    (if (Character/isUpperCase (first base-type))
      base-type
      {:todo v})))

#_(defn- type-name [element]
    (let [type-data (:type element)
          type-name (:code (first type-data))]
      (condp #(%1 %2) type-name
        upper-case?             type-name)))

(defn- element->gql-field-data-fn [struct-type]
  (fn [x]
    (let [cardinality (cardinality x)]
      (when (and (not (:sliceName x))
                 (not= :none cardinality))
        {:cardinality cardinality
         :type        (type-name (:type x))
         :field       (str/replace (:path x) (re-pattern (str "^" struct-type "\\.")) "")}))))

(defn- render-type [type-name fields]
  (str "type " type-name " {\n  "
       (str/join "\n  " (map (fn [{:keys [field cardinality type]}]
                               (str field ": " (case cardinality
                                                 :one      (str type "!")
                                                 :optional type
                                                 :many     (str "[" type "!]!")))) fields))
       "\n}"))

(defn structure-def->gql-type [json-str]
  (let [[head & tail] (:element (:snapshot (read-str json-str :key-fn keyword)))
        struct-type   (:path head)]
    (render-type struct-type (keep (element->gql-field-data-fn struct-type) tail))))
