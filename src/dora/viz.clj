(ns dora.viz
  (:require [clojure.string :as str]
            [digitalize.core :refer :all]
            [formaterr.core :refer :all]))

(defn buda
  [s]
  (:results (json (slurp (str "http://api.datos.gob.mx/v1/" s "?pageSize=10000")))))

(defn rkeys [rel]
  (keys (first rel)))

(defn pie [kx ky r]
  (let [vx (map kx r)
        ks (distinct vx)]
    (map #(hash-map :label % :value (reduce + (map ky (filter (fn [m] (= % (kx m)))
                                                              r))))
         ks)))

(defn viz [file data]
  (spit file (json data)))

(defn interactive-viz []
  (print "recurso? ")
  (let [recurso-name (read-line)]
    (println "Procesando recurso. . .")
    (let [recurso (buda recurso-name)
          llaves (rkeys recurso)]
      (println "llaves: " (str/join ", " (map name llaves)))
      (print "visualizacion? (pie) ")
      (let [la-viz (case (str/trim (read-line))
                     "pie" pie)]
        (print "eje X? ")
        (let [x (standard-keyword (read-line))]
          (print "eje Y? ")
          (let [y (standard-keyword (read-line))
                archivo (read-line)
                nombre-archivo (str recurso-name "-" (int (* 100 (rand))))]
              (viz nombre-archivo (la-viz x y recurso))))))))
