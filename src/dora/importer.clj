(ns dora.importer
  (:require [digitalize.core :refer :all]
            [dora.util :refer :all]
            [mongerr.core :refer [db-insert]]
            [nillib.formats :refer [csv]]))

(defn extract-csvs [folder]
  (let [files (ls-fr folder)]
    (map csv files)))

(defn import-csvs [collection folder]
  (doall (pmap #(db-insert collection %)
              (extract-csvs folder))))

(defn digest [collection folder digested errors]
  (doall (pmap #(try (db-insert collection (digitalize (csv %)))
                     (mv % digested)
                     (catch Exception e (db-insert :digest-error {:collection collection :exception e :file %})
                            (mv % errors)))
               (ls-fr folder))))

;(digest :test-profeco "/Users/nex/Desktop/testin/origin" "/Users/nex/Desktop/testin/destiny" "/Users/nex/Desktop/testin/errors")
