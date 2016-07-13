(ns dora.p.calificacion)

(defn broken-link-recommendation? [recommendations]
  (seq (filter #(= "La URL no pudo ser leída por el robot" (:name %))
                   recommendations)))

(defn falla-bronce [dataset resource recommendations]
  (or (empty? (:title dataset))
      (empty? (:description dataset))
      (empty? (:keyword dataset))
      (empty? (:theme dataset))
      (broken-link-recommendation? recommendations)))

(defn falla-plata [dataset resource recommendations]
  (or (nil? (-> dataset :publisher :name))
      (nil? (-> dataset :publisher :mbox))
      ; en el "algoritmo" original salia tmb funciona_la_url(dataset.publisher.url)
      ))

(defn falla-oro [dataset resource recommendations]
   ;cotejar contra la accrualPeriodicity
  (empty? recommendations))

(defn calificacion [adela recommendations]
  (let [dataset (:dataset adela)
        resource (:resource adela)]
    (cond (falla-bronce dataset resource recommendations) :none
          (falla-plata dataset resource recommendations) :bronce
          (falla-oro dataset resource recommendations) :plata
          :default :oro)))
