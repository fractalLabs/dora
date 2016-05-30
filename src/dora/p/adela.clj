(ns dora.p.adela
  (:require [monger.operators :refer :all]
            [mongerr.core :refer :all]))

(defn find-catalog-by-dataset-name
  "Search for the catalog that contains a dataset titled 'title'"
  [title]
  (db-findf :adela-catalogs {:dataset {$elemMatch {:title title}}}))
