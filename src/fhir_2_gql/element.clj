(ns fhir-2-gql.element
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [fhir-2-gql.util :refer :all]))

(defn path [element]
  (-> (:path element)
      (str/replace #"\[x\]$" "")
      (str/split #"\.")))

(defn cardinality [element]
  (match [(:min element) (:max element)]
    [0 "1"] :optional
    [1 "1"] :one
    [0 "0"] :none
    :else :many))

(defn- fhir->gql-type-name [type-info]
  (let [name (:code type-info)]
    (condp call name
      #{"id"}            "ID"
      #{"integer"
        "positiveInt"
        "unsignedInt"}   "Int"
      #{"time"
        "oid"
        "boolean"
        "decimal" ;; NOTE decimal is mapped to custom scalar type instead of Float because FHIR spec recommends using bigints instead of floats
        "date"
        "dateTime"
        "instant"
        "string"
        "uri"
        "markdown"
        "code"
        "base64Binary"}  (capitalize-first name)
      #{"xhtml"}         "String"
      #{"Reference"}     (if-let [referenced-url (first (or (:profile type-info)
                                                            (:targetProfile type-info)))]
                           (url->type-name referenced-url)
                           "Reference")
      first-capitalized? name)))

(defn element-types
  [element]
  (mapv fhir->gql-type-name (:type element)))

(defn value-set-url
  [element]
  (when-let [binding (:binding element)]
    (or (:reference (:valueSetReference binding))
        (:valueSetUri binding))))
