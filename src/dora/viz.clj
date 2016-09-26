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
       (map #(pie-format % ((friendly-resolve f) (map ky (filter (fn [m] (= % (m kx)))
                                                                 r))))
            ks)))))

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
