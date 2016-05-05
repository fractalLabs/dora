(ns dora.p.adela
  (:require [monger.operators :refer :all]
            [mongerr.core :refer :all]))

(defn find-catalog-by-dataset-name
  [title]
  (db-findf :adela-catalogs {:dataset {$elemMatch {:title title}}}))

(defn adela-dataset
  [title]
  )
