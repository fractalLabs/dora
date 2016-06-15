(ns dora.util
  "Various utility functions"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :refer :all]
            [clojure.string :as s]
            [clj-http.client :as http]
            [mongerr.core :refer :all]
            [nillib.formats :refer :all]))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  [f coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[x :as xs] seen]
                   (when-let [s (seq xs)]
                     (let [fx (f x)]
                       (if (contains? seen fx)
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen fx)))))))
                 xs seen)))]
    (step coll #{})))

(defn shs [& args]
  (let [result (apply sh args)]
    (println (:err result))
    ;(println (:exit result))
    (s/trim (:out result))))

(defn pull []
  (shs "git" "pull"))

(defn push []
  (shs "git" "push"))

(defn adda []
  (shs "git" "add" "-A"))

(defn commit
  ([] (commit "save"))
  ([msg] (shs "git" "commit" "-m" msg)))

(defn ggg []
  (adda)
  (commit)
  (push))

(defn gs []
  (shs "git" "status"))

(defn ls
  ([] (ls "."))
  ([dir]
   (.list (io/file dir))))

(defn mv
  "move a file from a to b"
  [a b]
  (let [response (shs "mv" a b)]
    (if (empty? response)
      :ok
      response)))

(defn ends-in-dash? [s]
  (= \/ (last s)))

(defn normalize-dir [dir]
  (if-not (ends-in-dash? dir)
    (str dir "/")
    dir))

(defn ls-fr
  "ls with full route"
  [dir]
  (map #(str (normalize-dir dir) %) (ls dir)))

(defn ls-fr-r
  "ls with full route recursive"
  [dir]
  (let [content (ls-fr dir)
        inside (mapcat ls-fr content)]
    (concat content inside)))

(defn remove-str [s match]
  (s/replace s match ""))

(defn is-directory?
  "Predicado para checar si el archivo es directorio"
  [route]
  (try
    (.isDirectory (io/file route))
    (catch Exception e false)))

(defn remove-transparencia-string
  [s]
  (remove-str (remove-str s  "=\" ") "\""))

(defn trim-transparencia-map
  [m]
  (zipmap (keys m)
          (map remove-transparencia-string (vals m))))

(defn trim-transparencia-csv [maps]
  (map trim-transparencia-map maps))

(defn feval                            ;TODO: security issue
  [s]
  (eval (read-string s)))

(defn clean-get
  [url]
  (:body (http/get url)))

(defn find-rel
  "Query kv from rel"
  [k v rel]
  (keep #(if (= (k %) v) %) rel))

(defn or*
  "Apply 'or' to a list of predicates"  ;Because u cant do
  [coll]                                ; (apply or [true true])
  (true? (some true? coll)))

(defn rel?
  "Is this a rel?"
  [o]
  (try (or* (map map? o))
       (catch Exception e false)))

(defn truthy?
  "Is this truthy or falsley"
  [o]
  (if o
    true
    false))

(defn map-vals
  "Apply f to the values of m"
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defn insert-csv
  "insert NAME.csv into collection NAME"
  [name]
  (db-insert name (csv (str name ".csv"))))
