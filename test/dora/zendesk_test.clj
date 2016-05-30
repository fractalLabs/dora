(ns dora.zendesk-test
  (:require  [clojure.test :refer :all]
             [dora.p.zendesk :refer :all]))

(deftest zendesk-connection
  (testing "Dora can connect to zendesk"
    (is (map? @(tickets)))))
