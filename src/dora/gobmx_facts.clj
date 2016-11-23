(ns dora.gobmx-facts)

(defn fact-de-recurso? [m]
  (re-find #"de bases de datos que la dependencia" (:fact m)))

(defn transforma-url [f fact]
  (assoc fact :url (f fact)))

(defn transforma-urls [f facts]
  (map #(transforma-url f %)
       facts))

(defn transforma-url-contadores [f facts]
  (concat (transforma-urls f (filter fact-de-recurso? facts))  (remove fact-de-recurso? facts)))
