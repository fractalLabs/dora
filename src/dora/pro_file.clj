(ns dora.pro-file
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.java.shell :refer :all]
            [clojure.repl :refer :all]
            [clojure.set :refer :all]
            [clojure.string :as s]
            [mongerr.core :refer :all]
            [dora.data :refer :all]
            [dora.p.adela :refer :all]
            [dora.p.agente-web :refer :all]
            [dora.util :refer :all]
            [nillib.formats :refer :all]
            [nillib.text :refer :all]
            [nillib.worm :refer :all]
            [ring.util.codec :as c]))

(defn shsh
  "Execute command in shell"
  [& command]
  (let [result (sh "/bin/sh" "-c" (s/join " " command))]
    (str (:err result) (:out result))))


(defmacro try-catch [body]
  `(try
     ~body
     (catch Exception e# (str "error " e#))))

(defn nils-csv-validation
  "Are there elements in a rel with too many nils?"
  [rel]
  (let [total (count (first rel))
        totals (map #(count (remove-nils (vals %))) rel)
        mini (apply min totals)]
    (if (> total 2)
      (if (> 3 mini)
        true
        false)
      false)))

(defn number-weird-format?
  "Does the number have a weird format
  such as '$' 'Kg' or anything that is not a number?"
  [s]
  (if-let [num (first (re-find number-regex s))]
    (not= s num)
    false))

(defn has-weird-format-numbers?
  [rel]
  (or* (map number-weird-format? (vals (first rel)))))

(defn rel-sample
  "take some random elements from the rel"
  ([rel]
    (rel-sample 20 rel))
  ([n rel]
    (take n (shuffle rel))))

(defn has-numbers?
  "The string has numbers in it?"
  [s]
  (if (re-find number-regex s)
      true
      false))

(defn vec-has-mixed-formats
  "Does a vector have a mix of numbers and strings?"
  [v]
  (apply not= (remove-nils (map has-numbers? v))))

(defn has-mixed-formats
  "Does the rel contain a mix of numbers and strings in some field?"
  [rel]
  (let [rel (rel-sample rel)
        ks (keys (first rel))]
    (or* (map vec-has-mixed-formats
              (map (fn [k] (map k rel))
                   ks)))))

(def metas
  "Vector of 'validations' or scripts whose output we collect"
  ["head -n 1" ;first line
   "file"      ;file type, encoding
   "wc -l"     ;line count
   "du -h"     ;file size
   "validaciones/IDMX/code/prep_proc"
   "validaciones/repetidos" ;conteo de lineas repetidas
   ])

(def csv-validations
  "Vector of validations aplicable only to rels generated from CSV"
  [{:name "nils-csv-validation" :fn nils-csv-validation}
   {:name "has-weird-format-numbers" :fn has-weird-format-numbers?}
   {:name "has-mixed-formats" :fn has-mixed-formats}])

(defn eval-csv-validation
  "Instead of just running the function, handle its exceptions"
  [f rel]
  (try (f rel)
       (catch Exception e "error")))

(defn csv-engine
  "Run validations specific to CSV"
  [file]
  (try
    (let [rel (take 1000 (csv file))]          ;just a 1000 rows sample for performance
      (zipmap (map :name csv-validations)
              (map #(eval-csv-validation % rel)
                   (map :fn csv-validations))))
    (catch Exception e {:csv-engine-error (str e)})))

(comment(defn dora-insert
          "Insert a newly read url into db"
          [url]
          (db-insert :dora {:url url :active true})))

(defn download-if-url
  "If name is a url, download and return relative path.
  nil if any error"
  [url]
  (try
    (if (re-find url-regex url)
      (let [file-name (str "/tmp/" (rand))
            tmp (spit file-name (clean-get url))]
        file-name)
      url)
    (catch Exception e)))

(defn execute-validations
  "Execution engine ;)"
  [file-name url]
  (remove-nils (merge (zipmap metas
                              (pmap #(try                   ;(if (string? %)
                                       (shsh % file-name)   ;    (% data))
                                       (catch Exception e (str e)))
                                    metas))
                      (if (re-find #"csv|CSV" url)
                        (csv-engine file-name)))))

(defn trim-macugly-line
  "Take the first line whith line breaks as \r
  whch are generated on old mac systems"
  [s]
  (re-find #"[^\r]+" s))

(defn trimming
  "Trim strings for head -n 1"
  [s]
  (s/join (take 500 (trim-macugly-line s))))

(defn first-numbers
  "Take only the first numeric substring"
  [s]
  (re-find #"[0-9]+" s))

(defn sfirst
  "second first"
  [coll]
  (second (first coll)))

(defn update-all-in
  "update-in with each key and fn pair"
  [m updates]
  (loop [m m
         updates updates]
    (if (empty? updates)
      m
      (recur (update-in m [(ffirst updates)] (sfirst updates))
             (rest updates)))))

(defn trim-file-script
  [s]
  (second (re-seq #"[^;]+" s)))

(defn process-validations
  "Post process validations, clean them and shit"
  [m]
  (update-all-in m {"head -n 1" trimming
                    "wc -l" first-numbers
                    "du -h" first-numbers
                    "validaciones/repetidos" first-numbers
                    "file" trim-file-script
                    }))

(defn to-validate []
  (difference (file-names) (set (map :id (db :resource-metadata)))))

(defn validate-dgm
  ([names]
   (doall (pmap #(db-update :resource-metadata
                            {:id %}
                            (assoc (process-validations (execute-validations (data-directory %) "csv")) :id %))
                names)))
  ([]
   (validate-dgm (to-validate))))

(defn validate-dgm-batched
  ([]
   (validate-dgm-batched (to-validate)))
  ([names] (loop [names names]
             (if (not (empty? names))
               (validate-dgm (take 100 names))
               (recur (drop 100 names))))))

(defn broken-link-recommendation
  "If the URL was reported as broken today, raise this alert"
  [url]
  (if-let [rec (broken-today url)]
    (assoc rec :name "La URL no pudo ser leída por el robot"
           :description "Esto puede significar que la URL está caída, o no sea accesible para robots."
           :more-info "http://datos.gob.mx/guia/publica/paso-2-1.html"
           :clave "d01")))

(def acento-regex #"[áéíóúÁÉÍÓÚñÑ]")

(defn acento-recommendation
  "Si la string tiene acentos, emite una recomendacion de no usar acentos en la URL"
  [s]
  (if (re-find acento-regex s)
    {:name "La URL contiene acentos"
     :description "Es recomendable que las urls no contengan acentos."
     :clave "i11"}))

(defn encoding-recommendation
  "Check file encoding against a list of blacklisted encodings"
  [metadata]
  (if (re-find #"8859" (metadata "file"))
    {:name "El archivo está en una codificación no universal"
     :description "Es recomendable utilizar la codificación UTF-8"
     :clave "i12"}))

(defn duplicated-url-recommendation
  "Checks if there are more than one resource with this url"
  [url]
  (if (> (count (re-find :resources {:url url})) 1)
    {:name "Hay mas de un recurso de datos con esta URL"
     :description "No es necesario que los mismos datos estén dados de alta mas de una vez. Revisar otras áreas, o dependencias que tengan estos datos publicados."
     :clave "c42"}))

(defn resource-description-recommendation
  "Checks if the resource has no description"
  [resource]
  (if (empty? (:description resource))
    {:name "El recurso no tiene descripción"
     :description "Es necesario agregar una descripción al recurso, para que sea fácil entender la finalidad del recurso"
     :clave "u22"}))

(defn has-mixed-formats-recommendation
  [m]
  (if (seq (filter #(and (re-find #"has_mixed_formats" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "El archivo contiene columnas en las que algunos elementos tienen números y otros no"
     :description "Evitar tener valores de diferentes tipos (como texto y número) para una columna en diferentes registros o filas.
"
     :clave "c41"}))

(defn nils-csv-validation-recommendation
  [m]
  (if (seq (filter #(and (re-find #"nils-csv-validation" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "El archivo contiene filas vacías"
     :description "Eliminar filas y columnas vacías, al igual que cálculos adicionales en formatos tabulares, p. ej. una celda “Total” con la suma de una de las columnas."
     :clave "m31"}))

(defn has-weird-format-numbers?-recommendation
  [m]
  (if (seq (filter #(and (re-find #"has-weird-format-numbers?" (:meta %))
                         (true? (:data %)))
                   m))
    {:name "Hay campos numéricos que contienen caracteres no numéricos"
     :description "Los campos numéricos, incluyendo los monetarios y magnitudes, deben permanecer en un formato numérico de tipo entero o flotante. Es decir, evitar agregar símbolos monetarios o de unidades de medición, p. ej. “200 m2” o “£20”, haciendo explícitas las unidades en el nombre de la columna, o en una segunda columna. P. ej. “Monto en MXN”, “Distancia en KM”."
     :clave "m33"}))

(comment
  (defn -recommendation
    [m]
    (if (seq (filter #(and (re-find #"" (:meta %))
                           (true? (:data %)))
                     m))
      {:name ""
       :description ""
       :clave ""})))

(defn recommendations
  "Generate recommendations from a file url and its metadata"
  [url metadata resource]
  (remove-nils (flatten [(try-catch (broken-link-recommendation url))
                         (try-catch (acento-recommendation url))
                         (try-catch (encoding-recommendation metadata))
                         (try-catch (duplicated-url-recommendation url))
                         (try-catch (resource-description-recommendation resource))
                         (if (and (not (nil? url)) (re-find #"csv|CSV" url))
                           [(try-catch (has-mixed-formats-recommendation metadata))
                            (try-catch (nils-csv-validation-recommendation metadata))
                            (try-catch (has-weird-format-numbers?-recommendation metadata))])])))

(defn resource
  "Find a resource with a URL"
  [url]
  (db-findf :resources {:url url}))

(defn resource-metadata
  "Find stored metadata for a id"
  [id]
  (db-findf :resource-metadata {:id id}))

(defn dataset
  "Given a Resource, return its dataset metadata"
  [resource]
  (db-findf :datasets {:id (:dataset_id resource)}))

(defn analytics
  "get the stored google analytics history"
  [url]
  (try (:value (db-findf :google_analytics {:url (c/url-encode url)}))
       (catch java.lang.NullPointerException e)))

(defn dora-view [result]
  (let [url (:url result)
        resource (resource url)
        dataset (dataset resource)
        catalog (find-catalog-by-dataset-name (:title dataset))
        catalog-dataset (first (find-rel :title
                                         (:title dataset)
                                         (:dataset catalog)))
        metadata (resource-metadata (:id resource))]
    {:resource resource
     :dataset (dissoc dataset :resources)
     :catalog (dissoc catalog :dataset)
     :catalog-dataset (dissoc catalog-dataset :distribution)
     :catalog-dataset-resource (first (find-rel :title
                                                (:name resource)
                                                (:distribution catalog-dataset)))
     :file-metadata metadata
     :recommendations (remove string?
                              (recommendations url metadata resource))}))

(def fusion dora-view)

(defn inventory-resources
  []
  (mapcat :distribution (mapcat :dataset (db-find :adela-inventories))))

(defn flatten-inventory [i]
  (map #(hash-map :dataset %
                  :inventory (dissoc i :dataset))
       (:dataset i)))

(defn flatten-inventory-dataset
  [d]
  (map #(hash-map :dataset (dissoc (:dataset d) :distribution)
                  :inventory (:inventory d)
                  :resource %)
       (:distribution (:dataset d))))

(defn inventory-resources-denormalized []
  (let [inventories (db :adela-inventories)
        datasets (mapcat flatten-inventory inventories)]
    (mapcat flatten-inventory-dataset datasets)))

(defn dora-view-inventory    ;will expect entries from inventory-resources-denormalized
  ([] (map dora-view-inventory (inventory-resources-denormalized)))
  ([result]
   (try (let [url (:downloadURL (:resource result))
              resource (resource url)
              dataset (dataset resource)
              metadata (resource-metadata (:id resource))]
          (assoc {:adela result}
                 :ckan {:resource resource
                        :dataset (dissoc dataset :resources)}
                 :analytics {:downloads {:total (analytics url)}}
                 :file-metadata metadata
                 :recommendations (remove string?
                                          (recommendations url metadata (:resource result)))
                 :id (str (:id (:dataset result)))))
        (catch Exception e (println "Exception: " e)))))

(defn save-fusion
  ([] (save-fusion (db :resources)))
  ([resources]
   (map #(try (db-insert :fusion (dora-view %))
              (catch Exception e (db-insert :fusion_errors
                                            (assoc % :exception (str e)))))
        resources)))

(defn save-fusion-inventory
  ([] (save-fusion-inventory (inventory-resources-denormalized)))
  ([results]
   (map #(try (db-insert :fusion_inventory (dora-view-inventory %))
              (catch Exception e (db-insert :errors_fusion_inventory
                                            (assoc % :exception (str e)))))
        results)))

(defn json-fusion-inventory []
  (let [rsrcs (inventory-resources-denormalized)
        n (count rsrcs)]
    (spit "data-fusion.json"
          (json (remove-nils (map #(try (dora-view-inventory %)
                                        (catch Exception e))
                                  rsrcs))))))

(comment
;;original validation engine
(defn profile
  "if first time, run validations and store.
  For returning costumers return previous results"
  ([url] (profile url metas))
  ([url metas]
   (if-let [result (resource-metadata url)]
     (dora-view result)
     (let [tmp (dora-insert url)
           file-name (download-if-url url)
           results (execute-validations file-name url)]
         (dora-update url results)
         (dora-view (dora-find url))))))

(defn folder-file
  "Concat the file name to the folder"
  [folder file]
  (if-not (= (last folder) \/)
          (str folder "/" file)
          (str folder file)))

(defn profile-folder
  "Run validations on all files from folder"
  [folder]
  (doall (pmap #(try (db-upsert :file-profiles
                                {:file %}
                                {:profile (profile %) :file %})
                     (catch Exception e (db-insert :error
                                                   {:error (str e)})))
               (map (partial folder-file folder)
                    (remove is-directory? (ls folder))))))

(defn validate
  "Validate a file.
  Main entry point."
  [file-name]
  (if (map? file-name)
    (validate (:url file-name))
    (if (is-directory? file-name)
      (profile-folder file-name)
      (profile file-name))))
)
