(ns ckan
  (:require  [clojure.test :refer :all]
             [dora.p.ckan :refer :all]))

(deftest dgm-ckan-connection
  (testing "Ensuring that we can retrieve stuff from ckan"
    (is (seq (package-list)))))
