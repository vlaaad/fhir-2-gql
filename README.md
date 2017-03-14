# Description

Library to convert fhir structure definitions to graphql schema definitions

# Usage

```clj
(require '[fhir-2-gql.core :refer :all])
(println (structure-def->schema-str (slurp (clojure.java.io/resource "examples/patient.profile.json"))))
=> type Patient {...}
   type PatientContact {...}
   ...

```

# TODO

- union types for some references
- enums from some codes
- abstract fhir types to graphql interfaces

# Tests

```
lein test
```
