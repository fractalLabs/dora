(ns dora.core-test
  (:require [clojure.test :refer :all]
            [dora.core :refer :all]
            [dora.util :refer :all]
            [mongerr.core :refer [db]]))

(deftest db-connection
  (testing "Ensuring that I can connect to db"
    (is (not (empty? (db))))))

(deftest preproc-rel
  (testing "Ensuring that preproc returns rels"
    (is (rel? (postproc "3" {:tipo :string})))
    (is (rel? (postproc {:a 2} {:tipo :map})))
    (is (rel? (postproc [1 2] {:tipo :coll})))))
