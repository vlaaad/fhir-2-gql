(ns fhir-2-gql.core
  (:require [clojure.data.json :refer [read-str]]
            [clojure.core.match :refer [match]]
            [fhir-2-gql.util :refer :all]
            [clojure.string :as str]))

;; processing

(defn cardinality [element]
  (match [(:min element) (:max element)]
    [0 "1"] :optional
    [1 "1"] :one
    [0 "0"] :none
    :else :many))

(defn- fhir->gql-type-name [type-info]
  (let [name (:code type-info)]
    (condp #(%1 %2) name
      #{"dateTime"
        "instant"}       "Instant" ;; TODO define Instant scalar type in overall gql schema
      #{"time"}          "Time"    ;; TODO define Time scalar type in overall gql schema
      #{"date"}          "Date"    ;; TODO define Date scalar type in overall gql schema
      #{"boolean"}       "Boolean"
      #{"id"}            "ID"
      #{"decimal"}       "Float"
      #{"integer"
        "positiveInt"
        "unsignedInt"}   "Int"
      #{"string"
        "code"
        "base64Binary"
        "markdown"
        "xhtml"
        "uri"
        "oid"}           "String"
      #{"Reference"}     (str/replace (first (:profile type-info)) #"^.+/" "")
      first-capitalized? name)))

(defn- add-type-declaration [acc type-name]
  (assoc acc type-name {:kind :type :fields []}))

;; TODO union types: detect when there are many field types and extract
;; TODO enums from codes
;; TODO do not fail on scalars

(defn- add-field-info [acc type-name field-info]
  (update-in acc [type-name :fields] (fn [coll]
                                       {:pre [(not (nil? coll))]}
                                       (conj coll field-info))))

(defn- override-type-name [type-name acc path]
  (if (#{"BackboneElement" "Element"} type-name)
    (let [type-name (str/join (map capitalize-first path))]
      [(add-type-declaration acc type-name) type-name])
    [acc type-name]))

(defn- process-element [acc element]
  (let [path (str/split (:path element) #"\.")]
    (if (= 1 (count path))
      (add-type-declaration acc (capitalize-first (first path)))
      (let [entity-type (str/join (map capitalize-first (butlast path)))
            field       (str/replace (last path) #"\[x\]$" "")
            cardinality (cardinality element)]
        (if (and (not (:sliceName element))
                 (:type element))
          (let [type-name (fhir->gql-type-name (first (:type element)))
                [acc type-name]       (override-type-name type-name acc path)]
            (add-field-info acc entity-type {:field       field
                                             :cardinality cardinality
                                             :type        type-name}))
          acc)))))

;; rendering

(defn- cardinality->gql
  [cardinality type]
  (case cardinality
    :one      (str type "!")
    :optional type
    :many     (str "[" type "!]!")))

;; api

(defn gql-type-map->schema-str [coll]
  (str/join
   "\n"
   (map
    (fn [[type-name {:keys [kind] :as data}]]
      (case kind
        :type (str "type " type-name " {\n  "
                   (str/join "\n  "
                             (->> (:fields data)
                                  (remove (comp #{:none} :cardinality))
                                  (map
                                   (fn [{:keys [field cardinality type]}]
                                     (str field ": " (cardinality->gql cardinality type))))))
                   "\n}")))
    coll)))

(defn structure-def->gql-type-map [json-str]
  (reduce process-element {} (:element (:snapshot (read-str json-str :key-fn keyword)))))

(defn structure-def->schema-str [json-str]
  (gql-type-map->schema-str (structure-def->gql-type-map json-str)))
