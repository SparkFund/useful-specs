(ns specs.number-test
  (:require [clojure.spec.test :as stest]
            [clojure.test :refer :all]
            [specs.number :refer :all]))

(deftest test-decimal-in
  (testing "generative"
    (is (every? nil? (map :failure (stest/check `decimal-in))))))
