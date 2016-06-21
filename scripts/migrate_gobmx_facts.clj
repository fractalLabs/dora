(ns migrate-gobmx-facts)

(def f (db :gobmx.facts))

(defn new-url [s]
  (str "http://datos.gob.mx/busca/dataset/" (last (re-seq #"[^/]+" s))))

(defn updater [fact]
  (update-in fact [:url] new-url))

(def f2 (map updater f))

(map #(db-update :gobmx.facts {:fact (:fact %)} %) f2)
