:organization :name


(def gacm (filter #(= "gacm" (-> % :organization :name)) data))

(def gacm (json (slurp "gacm-packages.json")))
(def gacm2 (:results (:result gacm)))
(def recursos (apply concat (map #(map (fn [r] (assoc r :ds-title (:title %)))
                                 (:resources %))
                           gacm2)))


(defn extrae-id [r]
  (first (re-seq #"[^ ]+" (:ds-title r))))

(def recursos-out (map #(assoc % :id (extrae-id %)) recursos))

(def recursos-sin-pdf (remove #(= "PDF" (:format %)) recursos-out))

(csv "recursos-gacm-sin-pdf.csv" recursos-sin-pdf)
