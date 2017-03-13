(defproject fhir-2-gql "0.1.0-SNAPSHOT"
  :description "convert fhir structure definitions to graphql ones"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [cpath-clj "0.1.2"]]
  :profiles {:dev {:dependencies [[com.graphql-java/graphql-java "2.3.0"]]
                   :resource-paths ["test-resources"]}})
