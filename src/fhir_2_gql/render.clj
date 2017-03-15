(ns fhir-2-gql.render
  (:require [clojure.string :as str]))
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
