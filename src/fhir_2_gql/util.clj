(ns fhir-2-gql.util
  (:require [clojure.string :as str]))

(defn capitalize-first [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))

(defn first-capitalized? [^CharSequence s]
  (Character/isUpperCase (first s)))

(defn call [f & args]
  (apply f args))

(defn url->type-name [url]
  (str/replace url #"^.+/" ""))

(defn path->type-name
  [path]
  (str/join (map capitalize-first path)))
