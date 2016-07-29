(ns fusion)

;compu1
(use 'dgm-analytics.core)
(def dl (download-data))

(def dl& (update-db :google-analytics #(identity dl)))

;compu2
(def fusion (update-db :data-fusion dora-view-inventory))


;compu1
(spit "data-fusion.json" (json (db :data-fusion)))

;compu1 buda
(def df (json (slurp "data-fusion.json")))
(def df& (update-db :data-fusion #(identity df)))
