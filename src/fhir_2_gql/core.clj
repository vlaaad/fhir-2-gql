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
      #{"boolean"}       "Boolean"
      #{"id"}            "ID"
      #{"decimal"}       "Float"
      #{"integer"
        "positiveInt"
        "unsignedInt"}   "Int"
      #{"oid"}           "Uri"
      #{"time"
        "date"
        "dateTime"
        "instant"
        "string"
        "uri"
        "base64Binary"}  (capitalize-first name)
      #{"code"
        "markdown"
        "xhtml"}           "String"
      #{"Reference"}     (str/replace (first (:profile type-info)) #"^.+/" "")
      first-capitalized? name)))

(defn- add-type-declaration [acc type-name]
  (assoc acc type-name {:kind :type :fields []}))

;; TODO union types: detect when there are many field types and extract
;; TODO enums from codes
;; TODO abstract types

(defn- add-field-info [acc type-name field-info]
  (update-in acc [type-name :fields] (fn [coll]
                                       {:pre [(not (nil? coll))]}
                                       (conj coll field-info))))

(defn- path->type-name
  [path]
  (str/join (map capitalize-first path)))

(defn- override-type-name [type-name acc path]
  (if (#{"BackboneElement" "Element"} type-name)
    (let [type-name (path->type-name path)]
      [(add-type-declaration acc type-name) type-name])
    [acc type-name]))

(defn- path
  [element]
  (str/split (:path element) #"\."))

(defn- scalar? [element]
  (and (:_code (first (:type element)))
       (= "value" (last (path element)))))

(defn- field? [element]
  (and (not (:sliceName element)) (:type element)))

(defn- process-element [acc element]
  (let [path (path element)]
    (if (= 1 (count path))
      (add-type-declaration acc (capitalize-first (first path)))
      (cond
        (scalar? element) (reduced {(path->type-name (butlast path)) {:kind :scalar}})
        (field? element)  (let [type-name       (fhir->gql-type-name (first (:type element)))
                                [acc type-name] (override-type-name type-name acc path)]
                            (add-field-info acc
                                            (path->type-name (butlast path))
                                            {:field       (str/replace (last path) #"\[x\]$" "")
                                             :cardinality (cardinality element)
                                             :type        type-name}))
        :else             acc))))

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
                   "\n}")
        :scalar (str "scalar " type-name)))
    coll)))

(defn structure-def->gql-type-map [json-str]
  (reduce process-element {} (:element (:snapshot (read-str json-str :key-fn keyword)))))

(defn structure-def->schema-str [json-str]
  (gql-type-map->schema-str (structure-def->gql-type-map json-str)))
