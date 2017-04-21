(ns dora.p.harvest
  (:require [clojure.string :as str]
            [dora.p.adela :refer :all]))

(def harvest-env-vars
  "CKAN_HOST=http:///172.16.156.233
  CKAN_API_TOKEN=ce08e9cf-62f2-4d74-b564-60c7f9a7229e
  CATALOG_HOST=http://adela.datos.gob.mx/api/v1/organizations/
  ")

(defn harvest-str& [slug]
  (str "python ckanops.py --harvest http://adela.datos.gob.mx/" slug "/catalogo.json"))

(defn harvest-str [slug]
  (str "docker run --name ckanop-manual -e CKAN_HOST=http://172.16.156.233 -e CKAN_API_TOKEN=ce08e9cf-62f2-4d74-b564-60c7f9a7229e -e CATALOG_HOST=http://adela.datos.gob.mx/api/v1/organizations/ mxabierto/ckanops:0.2 python ckanops.py --harvest http://adela.datos.gob.mx/" slug "/catalogo.json
docker rm ckanop-manual"))

(defn purge-harvest-str [slug]
  (str "docker run --name ckanop-manual -e CKAN_HOST=http://172.16.156.233 -e CKAN_API_TOKEN=ce08e9cf-62f2-4d74-b564-60c7f9a7229e -e CATALOG_HOST=http://adela.datos.gob.mx/api/v1/organizations/ mxabierto/ckanops:0.2 python ckanops.py --purge-harvest http://adela.datos.gob.mx/" slug "/catalogo.json
docker rm ckanop-manual"))

(defn massive-harvest []
  (spit "harvest-script.sh"
        (str harvest-env-vars
             (str/join "\n"
                       (map (comp harvest-str :slug)
                            (adela-api "api/v1/catalogs"))))))
