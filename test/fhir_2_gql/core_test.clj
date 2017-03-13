(ns fhir-2-gql.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cpath-clj.core :refer [resources]]
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

(deftest a-test
  (testing "gql-validator"
    (is (valid-gql? "type Foo {bar: Bar baz: Baz! beeps: [Beep]!}")))
  (testing "cardinality"
    (is (= :one (cardinality {:min 1 :max "1"})))
    (is (= :many (cardinality {:min 0 :max "*"})))
    (is (= :many (cardinality {:min 1 :max 2}))))
  (testing "structure-def->gql-type"
    (doseq [res (resources (io/resource "examples/"))]
      (is (valid-gql? (structure-def->gql-type (slurp (first (val res)))))))))
