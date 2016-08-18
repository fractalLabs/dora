(ns dora.p.adela
  (:require [clojure.string :as s]
            [dora.p.ckan :refer :all]
            [monger.operators :refer :all]
            [mongerr.core :refer :all]))

(def adela-url "http://adela.datos.gob.mx/")

(defn adela-api [& endpoint]
  (get-json (str adela-url
                 (s/join endpoint))))

(defn adela-catalog [slug]
  (assoc (adela-api slug "/catalogo.json")
         :slug slug))

;(defn adela-plan [slug]
;  (adela-api slug "/plan.json"))

(defn adela-catalogs []
  (map (comp adela-catalog :slug) (adela-api "api/v1/catalogs")))

;(defn adela-plans []
;  (remove nil? (map (comp adela-plan :slug) (adela-api "api/v1/catalogs"))))

(defn adela-inventory
  ([] (remove #(nil? (:title %))
              (map #(try (adela-inventory (:slug %))
                         (catch Exception e (println "error with: " (:slug %))))
                   (db :adela-organizations))))
  ([slug]
   (assoc (adela-api "api/v1/organizations/" slug "/inventory.json")
          :slug slug)))

(defn organizations-req
  ([] (organizations-req 1))
  ([page] (adela-api "api/v1/organizations?page=" page)))

(defn adela-organizations []
  (let [r1 (organizations-req)
        p (:pagination r1)
        last-page (int (Math/ceil (/ (:total p) (:per_page p))))]
    (apply concat (:results r1) (map #(:results (organizations-req %))
                                     (drop 2 (range (inc last-page)))))))

(defn find-catalog-by-dataset-name
  "Search for the catalog that contains a dataset titled 'title'"
  [title]
  (db-findf :adela-catalogs {:dataset {$elemMatch {:title title}}}))
