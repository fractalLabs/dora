(ns dora.viz
  (:require [clojure.string :as str]
            [digitalize.core :refer :all]
            [formaterr.core :refer :all]))

(defn buda
  [s]
  (:results (json (slurp (str "http://api.datos.gob.mx/v1/" s "?pageSize=10000")))))

(defn rkeys [rel]
  (keys (first rel)))

(defn reduce+ [coll]
  (reduce + coll))

(defmacro viz-variables []
  `(println "x: " x))

(defn pie
  ([kx ky r]
   (pie kx ky r reduce+))
  ([kx ky r f]
   (if (nil? f)
     (pie kx ky r reduce+)
     (let [vx (map kx r)
           ks (distinct vx)]
       (map #(hash-map :label % :value (f (map ky (filter (fn [m] (= % (kx m)))
                                                          r))))
            ks)))))

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
                          archivo (read-line)
                          nombre-archivo (str recurso-name "-" (int (* 100 (rand))))]
                      (viz-variables)
                      (spit-file nombre-archivo (pie x y recurso la-agregacion)))))))))))
