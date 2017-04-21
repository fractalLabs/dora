(ns dora.viz
  (:require [clojure.string :as str]
            [digitalize.core :refer :all]
            [formaterr.core :refer :all]))

(defn buda
  [s]
  (digitalize
   (:results (json (slurp (str "http://api.datos.gob.mx/v1/" s "?pageSize=10000"))))))

(defn rkeys [rel]
  (keys (first rel)))

(defn reduce+ [coll]
  (reduce + coll))

(defn pie-format [label value]
  {:label label :value value})

(defn friendly-resolve [o]
  (if (fn? o)
    o
    (ns-resolve *ns* (symbol o))))

(defn pie
  ([kx ky r]
   (pie kx ky r reduce+))
  ([kx ky r f]
   (if (nil? f)
     (pie kx ky r reduce+)
     (let [vx (map kx r)
           ks (distinct vx)]
       {:unidad (name ky)
        :ancho 550
        :valores
        (map #(pie-format % ((friendly-resolve f) (map ky (filter (fn [m] (= % (m kx)))
                                                                  r))))
             ks)}))))


(defn barchart [kx ky r f]
  (let [ks (distinct (map kx r))]
    {:ejex kx
     :ejey ky
     :valores
     (map #(zipmap [:label ky]
                   [% (f (map ky (filter (fn [el-r] (= % (kx el-r)))
                                         r)))])
          ks)}))

(defn spit-file [file data]
  (spit file (json data)))

(defn interactive-viz []
  (println "recurso? ")
  (let [recurso-name (read-line)]
    (println "Procesando recurso. . .")
    (let [recurso (buda recurso-name)
          llaves (rkeys recurso)]
      (println "llaves: " (str/join ", " (map name llaves)))
      (println "visualizacion? (pie) ")
      (let [la-viz (str/trim (read-line))]
        (case la-viz
          "pie" (do
                (println "agregacion: (default: reduce+) ")
                (let [la-agregacion (str/trim (read-line))]
                  (println "eje X? ")
                  (let [x (standard-keyword (read-line))]
                    (println "eje Y? ")
                    (let [y (standard-keyword (read-line))
                          nombre-archivo (str recurso-name "-x" x "-y" y "-grafica:pie-agregacion:" la-agregacion  ".json")]
                      (println "nombre-archivo: " nombre-archivo)
                      (println "x: " x)
                      (println "y: " y)
                      (spit-file nombre-archivo (pie x y recurso la-agregacion)))))))))))


;; TODO: filtering, "otros"

(comment
;;;; cosas especificas
(def retc (digitalize  (csv "/Users/nex/mirrors/retc/datos%20RETC.csv")))
(def retc-latest (filter #(= 2013 (:fecha %)) retc))
(def jeje (pie :estado :count retc-latest count))
(spit-file "puntos retc por estado 2013" jeje)

(def hotels (digitalize (csv "/Users/nex/mirrors/actividad-hotelera-por-entidad-federativa/6_1_td.csv")))
(def hotels2014 (filter #(= 2014 (:fecha %)) hotels))
(def hotelsviz (pie :estado :llegtur-tot hotels2014))
(spit-file "/Users/nex/git/dataviz/piechart/partials/llegadas a hoteles 2014.json" hotelsviz)


(def pueblos (digitalize (csv "/Users/nex/mirrors/tabla-de-las-localidades-que-cuentan-con-el-nombramiento-de-pueblo-magico/24-Localidades-que-cuentan-con-el-nombramiento-de-Pueblo-Magico.csv")))
(def pueblosviz (pie :estado :ja pueblos count))
(spit-file "/Users/nex/git/dataviz/piechart/partials/numero de pueblos magicos por estado.json" pueblosviz)

(def matricula-escuelas (digitalize (csv "/Users/nex/mirrors/matricula-por-institucion-y-entidad-federativa/matriculaporinstitucionyentidadfederativa.csv")))
(def escuelasviz (pie :entidad-federativa :ja matricula-escuelas count))
(spit-file "/Users/nex/git/dataviz/piechart/partials/escuelas por estado.json" escuelasviz)

(def sostenimientoviz (pie :sostenimiento :ja matricula-escuelas count))
(spit-file "/Users/nex/git/dataviz/piechart/partials/tipo de sostenimiento de instituciones educativas.json" sostenimientoviz)

(def concesiones (digitalize (csv "/Users/nex/mirrors/tabla-de-las-localidades-que-cuentan-con-el-nombramiento-de-pueblo-magico/24-Localidades-que-cuentan-con-el-nombramiento-de-Pueblo-Magico.csv")))

(def calidadagua (digitalize (csv "/Users/nex/mirrors/datos-de-calidad-del-agua-de-5000-sitios-de-monitoreo/Calidad_del_Agua_2014_3I.csv" )))
(def calidadviz (pie :sst :ja calidadagua count))
(spit-file "/Users/nex/git/dataviz/piechart/partials/calidad general del agua por estacion.json" calidadviz)

(def estacionesxestado (pie :estado :ja calidadagua count))
(spit-file "/Users/nex/git/dataviz/piechart/partials/estaciones de calidad del agua por estado.json" estacionesxestado)

(def residuos (digitalize (csv "/Users/nex/mirrors/residuos-solidos-urbanos-manejados-adecuadamente/Residuos s%F3lidos urbanos manejados adecuadamente datos.csv")))
(def residuosviz (pie :entidad-federativa :2015 matricula-escuelas))
(spit-file "/Users/nex/git/dataviz/piechart/partials/residuos solidos manejados adecuadamente por estado 2015.json" residuosviz)

(def residuosviz2014 (pie :entidad-federativa :2014 matricula-escuelas))
(spit-file "/Users/nex/git/dataviz/piechart/partials/residuos solidos manejados adecuadamente por estado 2014.json" residuosviz2014)

(def rezago-social-estatal (digitalize (csv "/Users/nex/mirrors/rezago-social-estatal/Rezago-social-2000-2005-2010_edos_DA.csv")))
(def rezagoviz (pie :ent :pobtot-10 rezago-social-estatal))
(spit-file "/Users/nex/git/dataviz/piechart/partials/poblacion por estado 2010.json" rezagoviz)
)
