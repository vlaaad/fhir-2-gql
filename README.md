# Description

Library to convert fhir structure definitions to graphql schema definitions

# Usage

```clj
(require '[fhir-2-gql.core :refer :all])
(println (structure-def->schema-str (slurp (clojure.java.io/resource "examples/patient.profile.json"))))
=> type Patient {...}
   type PatientContact {...}
   ...

;; You can set some `code`-types to custom enum types by passing map of value set uris to enum type names
;; Generating GraphQL enums from these uris is out of scope of this project
(println (structure-def->schema-str
          (slurp (clojure.java.io/resource "examples/structuredefinition.profile.json"))
          {"http://tools.ietf.org/html/bcp47" "Language"}))
=> type StructureDefinition implements DomainResource {
     ...
     language: Language
     ...
   }
   ...

```

# Tests

```
lein test
```
