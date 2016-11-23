(ns dora.mapa
  (:require [clojure.string :as str]
            [digitalize.core :refer :all]
            [formaterr.core :refer :all]
            [mongerr.core :refer :all]))
(def mx (json (slurp "/Users/nex/git/dgm-viz/visualizaciones/mapa/partials/mxGeo.json")))

;(keys mx)
;(:type :etiqueta-info :etiqueta-pop :ejex :ejey :features)

(def features (:features mx))

;;(count features)
;;32

;;dora.repl=> (def ffeature (first features))
;;#'dora.repl/ffeature
;;dora.repl=> (class ffeature)
;;clojure.lang.PersistentArrayMap
;;dora.repl=> (keys ffeature)
;;(:type :geometry :properties)

(defn merge-structure [estado eststruct]
  (assoc eststruct :properties (merge (:properties eststruct) estado)))

(defn add-structure [estados]
  (let [structure (db :map-shape-template)]
    (map #(merge-structure  %
                            (first (filter (fn [estado] (re-find (re-pattern (standard-name (:estado %))) (standard-name (:estado (:properties estado)))))
                                           structure)))

         estados)))

;(join (db :estados-geo) estados)

(def monitoreo (csv "/Users/nex/mirrors/monitoreo-de-calidad-de-agua-de-uso-y-consumo-humano/Agua_Calidad.csv"))

(defn transform-monitoreo [monitoreo]
  (map #(assoc % :estado (str/capitalize (:estado %)))
       monitoreo))

(defn limpia-edo [s] (str/join (drop 6 s)))

(def meses {"enero" 1
            "febrero" 2
            "marzo" 3
            "abril" 4
            "mayo" 5
            "junio" 6
            "julio" 7
            "agosto" 8
            "septiembre" 9
            "octubre" 10
            "noviembre" 11
            "diciembre" 12})

(defn limpia-mes [s] (get meses (standard-name s)))

(defn totales-estado []
  (map #(assoc % :estado (standard-name (limpia-edo (:estado %)))
                 :fecha (limpia-mes (:mes %)))
       (filter #(re-find #"total" (standard-name (:estado %)))
               (transform-monitoreo monitoreo))))

(def tot-est (totales-estado))

(defn grafica-mapa [data value-key]
  (map #(hash-map :tiempo (:fecha %) :valor (value-key %))
       data))

(def gra)
(defn map-properties [valor porc grafica estado]
  {:valor_info valor
   :porcentaje_pop porc
   :grafica grafica
   :estado estado})

(defn map-grafica [k-valor data]
  (map #(hash-map :tiempo (:fecha %)
                  :valor (k-valor %))
       data))

(defn feature-out [identit k-identity k-valor k-porc k-grafica]
  (let [data (filter #(= identit (k-identity %))
                     tot-est)
        valid (first (sort-by :fecha compare data))]
    (map-properties (k-valor valid)
                    (k-porc valid)
                    (map-grafica k-grafica data)
                    (k-identity (first data)))))


(defn todo-monitoreo [k-identity k-valor k-porc k-grafica]
  (add-structure (transform-monitoreo
                  (map #(feature-out  % k-identity k-valor k-porc k-grafica)
                       (distinct (map k-identity tot-est))))))
;; add-structure, to it


;; meses
;; llaves para map feature
;; wrapup

(def monitoreo-cloro
  (todo-monitoreo :estado :cobertura-de-vigilancia :porcentaje-de-poblacion-de-la-entidad-federativa-con-cobertura-de-vigilancia :porcentaje-de-poblacion-de-la-entidad-federativa-con-cobertura-de-vigilancia))

(defn formato-mapa [properties data]
  (assoc properties
         :type "FeatureCollection"
         :features data))

(defn genera-mapa [k-identity k-valor k-porc k-grafica]
  (formato-mapa {:etiqueta_info (str (standard-name k-valor) " por estado") :etiqueta_pop (standard-name k-porc) :ejex :fecha :ejey (standard-name k-grafica)}
                (todo-monitoreo k-identity k-valor k-porc k-grafica)))

;;(def mm (genera-mapa :estado :cobertura-de-vigilancia :porcentaje-de-poblacion-de-la-entidad-federativa-con-cobertura-de-vigilancia :porcentaje-de-poblacion-de-la-entidad-federativa-con-cobertura-de-vigilancia))
;;(def mmm (assoc mm :features (add-structure (:features mm))))
;;(json "/Users/nex/git/dgm-viz/visualizaciones/mapa/partials/cobertura-de-vigilancia.json" mmm)


;;(def mm (genera-mapa :estado :dentro-de-norma :eficiencia-de-cloracion :dentro-de-norma))
;;(def mmm (assoc mm :features (add-structure (:features mm))))
;;(json "/Users/nex/git/dgm-viz/visualizaciones/mapa/partials/dentro-de-norma.json" mm)


;;(def mm (genera-mapa :estado :debajo-de-norma :dentro-y-arriba-de-norma :dentro-y-arriba-de-norma))
;;(def mmm (assoc mm :features (add-structure (:features mm))))
;;(json "/Users/nex/git/dgm-viz/visualizaciones/mapa/partials/debajo-de-norma.json" mm)


;;bugs bunny
;;(def mm (genera-mapa :estado :total-de-muestras-para-analisis-bacteriologico :eficiencia-de-cloracion :muestras-aptas-para-consumo))
;;(def mmm (assoc mm :features (add-structure (:features mm))))
;;(json "/Users/nex/git/dgm-viz/visualizaciones/mapa/partials/muestras-para-analisis-bacteriologico.json" mm)
