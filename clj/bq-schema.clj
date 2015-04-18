(ns bq.schema
  (:require [cheshire.core :refer :all]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clojure.pprint :refer (pprint)]
            [clojure.string :refer (join)]))

(def raw-doc (parse-string (slurp "../luffer/tmp/one.json")))

(def fmt (f/formatters :date-time-no-ms))

(defn- datey? [v]
  (re-matches #"\d{4}-\d{2}-\d{2}.*" v))

(defn- detect-type [v]
  (cond
   (nil? v) "STRING"
   (or (instance? Boolean v)) "BOOLEAN"
   (or (instance? Double v)) "FLOAT"
   (or (instance? Long v) (instance? Integer v)) "INTEGER"
   (or (vector? v) (map? v) (list? v)) "RECORD"
   (datey? v)           "TIMESTAMP" 
   (instance? String v) "STRING"))

;; (defn- schema-field [keys type]
;;   (join ":" [(join "." keys) type]))

(declare mark-types)

(defn- schema-field [k v]
  (let [json {"name" k
              "type" (detect-type v)}]
    (if (map? v)
      (merge json {"fields" (mark-types v)})
      json)))

(defn mark-types [hsh]
  (vec (map (fn [[k v]]
              (schema-field k v))
            hsh)))

(defn make-schema [json]
  (join "," (mark-type [] json)))

(spit "schema3.json" (generate-string (mark-types raw-doc)))
;; (defn mark-type [json]
;;   (map
;;    (cond
;;     (detect-type v))
;;    json))
