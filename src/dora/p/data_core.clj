(ns dora.p.data-core
  "Copy and update the collections that are used in data fusion"
  (:require [chime :refer [chime-ch]]
            [clj-time.core :as t]
            [clj-time.periodic :refer [periodic-seq]]
            [clojure.core.async :as a :refer [<! go-loop]]
            [clojure.set :refer :all]
            [clojure.string :as s]
            ;[dgm-analytics.core :refer :all]
            [dora.data :refer :all]
            [dora.p.adela :refer :all]
            [dora.p.ckan :refer :all]
            [dora.p.zendesk :refer :all]
            [dora.pro-file :refer :all]
            [monger.operators :refer :all]
            [mongerr.core :refer :all]
            [nillib.formats :refer :all]
            [nillib.worm :refer :all])
  (:import [org.joda.time DateTimeConstants DateTimeZone]))

(defn dc-add-query
  [campo value-fn]
  (db-update :data-core
             {:campo campo}
             {:query-en-data-core value-fn}))

(defn dc-update
  ([campo value-fn]
   (try
     (db-update :data-core
                {:campo campo}
                {:value  (if (fn? value-fn)
                           (value-fn)
                           (eval (read-string value-fn)))})
     (catch Exception e (println "caught-exception: dc-update ->>"))))
  ([e]
   (dc-update (:campo e) (:query-en-data-core e)))
  ([]
   (map dc-update  (remove #(nil? (:query-en-data-core %)) (db-find :data-core)))))

(def drive-files {:instituciones "https://docs.google.com/feeds/download/spreadsheets/Export?key=1swzmgetabUT25eog-g6pdgRlc8x9uqz3iCNoruhdnxE&exportFormat=csv&gid=2050308732"
                  :ipda "https://docs.google.com/feeds/download/spreadsheets/Export?key=1swzmgetabUT25eog-g6pdgRlc8x9uqz3iCNoruhdnxE&exportFormat=csv&gid=1077082165"})

(defn parse-drive [m]
  (zipmap (keys m) (map (comp digitalize csv) (vals m))))

(defn drive [] (parse-drive drive-files))

(defn instituciones []
  (let [d (drive)]
    (map #(dissoc % :slug) (join (:instituciones d) (:ipda d) {:siglas :slug}))))

(defn update-db [coll f]
  (do (db-delete coll)
      (db-insert coll (remove-nils (f)))))

(defn data-core []
  (doall [(update-db :instituciones instituciones)
          (update-db :zendesk-tickets all-tickets)
          (update-db :zendesk-organizations all-organizations)
          (update-db :zendesk-satisfaction all-satisfaction)
          (update-db :zendesk-users all-users)
          (update-db :adela-catalogs adela-catalogs)
          (update-db :adela-plans adela-plans)
          (update-db :adela-organizations adela-organizations)
          (update-db :adela-inventories adela-inventory)
                                        ;(update-db :google_analytics download-data)
          (mv-old-file)
          (validate-dgm)
          (update-db :fusion_inventory dora-view-inventory)
          (dc-update)]))

(defn today-at
  ([] (today-at 0 0 0 0))
  ([h] (today-at h 0 0 0))
  ([h m] (today-at h m 0 0))
  ([h m s] (today-at h m s 0))
  ([h m s mil]
   (.. (t/now)
       (withZone (DateTimeZone/forID "America/Mexico_City"))
       (withTime h m s mil))))
(chime-at [(-> 2 t/secs t/from-now) (-> 4 t/secs t/from-now)]

          (fn [time]
            (println "Chiming at" time))

          {:on-finished (fn []
                          (println "Schedule finished."))})
(defn schedule
  [time lapse f]
  (chime-at (periodic-seq (apply today-at time)
                          lapse)
            (fn [time]
              (f)
              (println "At: " time))
            {:error-handler (fn [e] (println "at: " (t/now) ", Error: " e))}))

(defn daily-schedule
  ([f]
   (daily-schedule [] f))
  ([time f]
   (schedule time (t/days 1) f)))

(defn schedule-data-core
  "Run data core everyday at 12am"
  []
  (daily-schedule data-core))

(defn metricas
  "Despliega las Métricas de Data Core"
  []
  (map #(vector (:value %) (:campo %))
       (db-find :data-core {:value {$exists true}})))
