(ns fhir-2-gql.core
  (:require [clojure.data.json :refer [read-str]]
            [clojure.string :as str]
            [fhir-2-gql
             [element :as el]
             [render :refer [gql-type-map->schema-str]]
             [util :refer :all]]))

(def graphql-scalar-types #{"Int" "Float" "String" "Boolean" "ID"})

;; map filling

(defn- add-type-declaration [acc type-name abstract base]
  (assoc acc type-name {:kind     :type
                        :abstract abstract
                        :base     base
                        :fields   []}))

(defn- add-union-type [acc type-name types]
  (assoc acc type-name {:kind :union
                        :types (set types)}))

(defn- add-field [acc type-name field-info]
  (if (contains? acc type-name)
    (update-in acc [type-name :fields] conj field-info)
    acc)) ;; NOTE: there may be no existing type declaration on constrained types with constraints on inner fields of concrete types (see cholesterol.example.json -> valueQuantity). This library ignores such constraints

;; type processing

(defn- embedded-type? [type-names]
  (#{["BackboneElement"] ["Element"]} type-names))

(defn union-type? [type-names]
  ;; NOTE: covers 2 cases: references to values of either values and embedded "[x]"-values
  (> (count type-names) 1))

(defn simple-type? [type-names]
  (= (count type-names) 1))

(defn- process-type-names [type-names acc path]
  (condp call type-names
    embedded-type? (let [type-name (path->type-name path)]
                     [(add-type-declaration acc type-name false nil) type-name])
    union-type?    (let [type-name (path->type-name path)]
                     [(add-union-type acc type-name type-names) type-name])
    simple-type?   [acc (first type-names)]))

;; structure processing

(defn- scalar? [path element]
  (and (:_code (first (:type element)))
       (= "value" (last path))))

(defn- field? [path element]
  (:type element))

(defn- type-decl? [path element]
  (= 1 (count path)))

(defn- slice? [path element]
  (:sliceName element))

(defn- enum? [enums]
  (fn [path element]
    (and (= ["Code"] (el/element-types element))
         (when-let [url (el/value-set-url element)]
           (contains? enums url)))))

(defn- process-element-rf [structure-def enums]
  (let [major-type-name (->> (or (:name (first (:element (:snapshot structure-def))))
                                 (:name structure-def))
                             (re-seq #"\w+")
                             (map capitalize-first)
                             str/join)
        base-type-name  (or (:constrainedType structure-def)
                            (some-> (or (:base structure-def)
                                        (:baseDefinition structure-def))
                                    url->type-name))]
    (fn [acc element]
      (let [path (assoc (el/path element) 0 major-type-name)]
        (condp apply [path element]
          type-decl?    (add-type-declaration acc
                                              (capitalize-first (first path))
                                              (:abstract structure-def)
                                              base-type-name)
          slice?        acc
          scalar?       (let [scalar-type-name (path->type-name (butlast path))]
                          (reduced (if (graphql-scalar-types scalar-type-name)
                                     {}
                                     {scalar-type-name {:kind :scalar}})))
          (enum? enums) (add-field acc
                                   (path->type-name (butlast path))
                                   {:field       (last path)
                                    :cardinality (el/cardinality element)
                                    :type        (get enums (el/value-set-url element))})
          field?        (let [[acc type-name] (process-type-names
                                               (el/element-types element)
                                               acc
                                               path)]
                          (add-field acc
                                     (path->type-name (butlast path))
                                     {:field       (last path)
                                      :cardinality (el/cardinality element)
                                      :type        type-name}))
          acc)))))

;; api

;; NOTE: GraphQL spec does not say if types can implement other concrete types in addition to interfaces, but existing implementations (graphql-java, for example) imply that types can implement only interfaces. This library may generate types that implement other concrete types (Age -> Quantity, for example).
(defn structure-def->gql-type-map
  "enums is map, where keys are urls/uris of value sets referenced by element bindings"
  ([json-str]
   (structure-def->gql-type-map json-str nil))
  ([json-str enums]
   (let [structure-def (read-str json-str :key-fn keyword)]
     (reduce (process-element-rf structure-def enums) {} (:element (:snapshot structure-def))))))

(defn structure-def->schema-str
  ([json-str]
   (structure-def->schema-str json-str nil))
  ([json-str enums]
   (gql-type-map->schema-str (structure-def->gql-type-map json-str enums))))
