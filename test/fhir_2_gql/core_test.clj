(ns fhir-2-gql.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cpath-clj.core :refer [resources]]
            [fhir-2-gql.element :refer :all]
            [fhir-2-gql.core :refer :all])
  (:import graphql.parser.Parser
           java.util.concurrent.CancellationException))

(defn example-json [name]
  (slurp (io/resource (str "examples/" name ".json"))))

(defn- valid-gql? [str]
  (try
    (boolean (.parseDocument (Parser.) str))
    (catch CancellationException e
      false)))

(deftest element-test
  (testing "cardinality"
    (is (= :one (cardinality {:min 1 :max "1"})))
    (is (= :many (cardinality {:min 0 :max "*"})))
    (is (= :many (cardinality {:min 1 :max 2})))))

(deftest gql-test
  (testing "gql-validator"
    (is (valid-gql? "type Foo {bar: Bar baz: Baz! beeps: [Beep]!}")))
  (testing "structure-def->gql-type"
    (doseq [res (resources (io/resource "examples/"))]
      (let [json-str (slurp (first (val res)))]
        (is (or (valid-gql? (structure-def->schema-str json-str))
                (empty? (structure-def->schema-str json-str))) (key res))))))

(defn- field-type [gql-type-map type-name field-name]
  (->> (get-in
        gql-type-map
        [type-name :fields])
       (filter (comp #{field-name} :field))
       first
       :type))

(deftest enum-test
  (let [lang-type-of-struct (fn [enums]
                              (field-type
                               (structure-def->gql-type-map (example-json "structuredefinition.profile") enums)
                               "StructureDefinition"
                               "language"))]
    (is (= "Code" (lang-type-of-struct nil)))
    (is (= "Language" (lang-type-of-struct {"http://tools.ietf.org/html/bcp47" "Language"})))))
