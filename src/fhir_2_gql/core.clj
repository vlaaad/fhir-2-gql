(ns fhir-2-gql.core
  (:require [clojure.data.json :refer [read-str]]
            [clojure.core.match :refer [match]]
            [fhir-2-gql.util :refer :all]
            [clojure.string :as str]))

(def graphql-scalar-types #{"Int" "Float" "String" "Boolean" "ID"})

;; processing

(defn cardinality [element]
  (match [(:min element) (:max element)]
    [0 "1"] :optional
    [1 "1"] :one
    [0 "0"] :none
    :else :many))

(defn- url->type-name [url]
  (str/replace url #"^.+/" ""))

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
        "xhtml"}         "String"
      #{"Reference"}     (if-let [referenced-url (first (or (:profile type-info)
                                                            (:targetProfile type-info)))]
                           (url->type-name referenced-url)
                           "Reference")
      first-capitalized? name)))

(defn- add-type-declaration [acc type-name abstract base]
  (assoc acc type-name {:kind     :type
                        :abstract abstract
                        :base     base
                        :fields   []}))

(defn- add-union-type [acc type-name types]
  (assoc acc type-name {:kind :union
                        :types types}))

(defn- add-field-info [acc type-name field-info]
  (update-in acc [type-name :fields] (fn [coll]
                                       {:pre [(not (nil? coll))]}
                                       (conj coll field-info))))

(defn- path->type-name
  [path]
  (str/join (map capitalize-first path)))

(defn- embedded-type? [type-names]
  (#{["BackboneElement"] ["Element"]} type-names))

(defn union-type? [type-names path]
  ;; NOTE: covers 2 cases: references to multiple and "[x]"-values
  (> (count type-names) 1))

(defn simple-type? [type-names]
  (= (count type-names) 1))

(defn- process-type-names [type-names acc path]
  (condp #(%1 %2) type-names
    embedded-type?        (let [type-name (path->type-name path)]
                            [(add-type-declaration acc type-name false nil) type-name])
    #(union-type? % path) (let [type-name (path->type-name path)]
                            [(add-union-type acc type-name type-names) type-name])
    simple-type?          [acc (first type-names)]))

(defn- path [element]
  (str/split (str/replace (:path element) #"\[x\]$" "") #"\."))

(defn- scalar? [element]
  (and (:_code (first (:type element)))
       (= "value" (last (path element)))))

(defn- field? [element]
  (and (not (:sliceName element)) (:type element)))

(defn- type-decl? [element]
  (= 1 (count (path element))))

(defn- process-element-rf [structure-def]
  (fn [acc element]
    (let [path (path element)]
      (cond
        (type-decl? element) (add-type-declaration acc
                                                   (capitalize-first (first path))
                                                   (:abstract structure-def)
                                                   (some-> (or (:base structure-def)
                                                               (:baseDefinition structure-def))
                                                           url->type-name))
        (scalar? element)    (let [scalar-type-name (path->type-name (butlast path))]
                               (reduced (if (graphql-scalar-types scalar-type-name)
                                          {}
                                          {scalar-type-name {:kind :scalar}})))
        (field? element)     (let [[acc type-name] (process-type-names
                                                    (mapv fhir->gql-type-name (:type element))
                                                    acc
                                                    path)]
                               (add-field-info acc
                                               (path->type-name (butlast path))
                                               {:field       (last path)
                                                :cardinality (cardinality element)
                                                :type        type-name}))
        :else                acc))))

;; rendering

(defn- render-cardinality
  [cardinality type]
  (case cardinality
    :one      (str type "!")
    :optional type
    :many     (str "[" type "!]!")))

(defn- render-type [type-name {:keys [abstract fields base]}]
  (str (if abstract "interface" "type") " " type-name " "
       ;; NOTE: graphql interfaces can't implement other interfaces
       (when (and (not abstract) base) (str "implements " base " ")) "{\n  "
       (str/join "\n  "
                 (->> fields
                      (remove (comp #{:none} :cardinality))
                      (map
                       (fn [{:keys [field cardinality type]}]
                         (str field ": " (render-cardinality cardinality type))))))
       "\n}"))

;; api

(defn gql-type-map->schema-str [coll]
  (str/join
   "\n"
   (map
    (fn [[type-name {:keys [kind] :as data}]]
      (case kind
        :type (render-type type-name data)
        :scalar (str "scalar " type-name)
        :union (str "union " type-name " = " (str/join " | " (:types data)))))
    coll)))

(defn structure-def->gql-type-map [json-str]
  (let [structure-def (read-str json-str :key-fn keyword)]
    (reduce (process-element-rf structure-def) {} (:element (:snapshot structure-def)))))

(defn structure-def->schema-str [json-str]
  (gql-type-map->schema-str (structure-def->gql-type-map json-str)))
