(ns dora.refineria
  (:require [clojure.java.shell :refer :all]
            [digitalize.core :refer :all]
            [dora.pro-file :refer :all]
            [dora.util :refer :all]
            [environ.core :refer [env]]
            [formaterr.core :refer :all]
            [tentacles.repos :refer [create-org-repo]]))

;; Por ahora los recursos vip. despues sera (db :resources)
(def mirrored-resources (map resource ["http://web.coneval.gob.mx/Informes/Pobreza/Datos_abiertos/Rezago-social-2000-2005-2010_mun_DA.csv" "http://www.cofepris.gob.mx/Transparencia/Documents/datos_abiertos/Agua_Calidad.csv" "http://201.116.60.46/DatosAbiertos/Datos_de_calidad_del_agua_de_5000_sitios_de_monitoreo.zip" "http://www.siap.gob.mx/datosAbiertos/Estadist_Produc_Agricola/Agt_cierre_2013_2014.csv" "http://www.datos.economia.gob.mx/RegulacionMinera/ConcesionesMineras.csv" "http://dsiappsdev.semarnat.gob.mx/datos/aire/datos%20RETC.csv" "http://www.sectur.gob.mx/DATOS/0116/19-Inventario-Turistico-por-entidad-federativa.csv" "http://www.sectur.gob.mx/DATOS/0116/24-Localidades-que-cuentan-con-el-nombramiento-de-Pueblo-Magico.csv" "http://www.datatur.beta.sectur.gob.mx/Documentos%20compartidos/6_1_td.csv" "http://catalogo.datos.gob.mx/dataset/54ae9a90-418c-4088-88d0-edea59814826/resource/ffc1323a-bf46-4d9d-86a8-237315c2036e/download/matriculaporinstitucionyentidadfederativa.csv" "http://data.sct.gob.mx:8082/datos/datos/abiertos/11601MexicoConectado.xlsx" "http://www.correosdemexico.gob.mx/datosabiertos/cp/cpdescarga.txt" "http://www.correosdemexico.gob.mx/datosabiertos/poligonos/mapapoligonos.zip"]))

(def ^:dynamic refineria-dir "/Users/nex/mirrors/")
(def ^:dynamic gh-org "mxabierto-mirror/")

(defn clone-mirror [resource]
  (let [le-name (standard-name (:name resource))]
    (clone (str gh-org le-name)
           (str refineria-dir le-name))))

(defn repo-mirror [resource]
  (let [le-name (standard-name (:name resource))]
    (create-org-repo "mxabierto-mirror"
                     le-name
                     {:auth (env :gh)
                      :description (:description resource)})))

(defn repo [resource]
  (try (clone-mirror resource)
       (catch Exception e
         (repo-mirror resource)
         (clone-mirror resource))))

(defn checkout
  ([dir]
   (checkout dir "refineria"))
  ([dir branch]
   (sh "git" "checkout" "-b" branch :dir dir)))
