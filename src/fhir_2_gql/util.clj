(ns fhir-2-gql.util)

(defn capitalize-first [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))

(defn first-capitalized? [^CharSequence s]
  (Character/isUpperCase (first s)))
