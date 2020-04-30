(ns poker-problem.core-test
  (:require [clojure.test :refer :all]
            [poker-problem.core :refer :all]))

(deftest best-hands
  (testing "Make sure the problem hands all produce same result"
    (is (= :high-card (best-hand ["HA" "D2" "H3" "C9" "DJ"])))
    (is (= :pair (best-hand ["HA" "HQ" "SJ" "DA" "HT"])))
    (is (= :two-pair (best-hand ["HA" "DA" "HQ" "SQ" "HT"])))
    (is (= :three-of-a-kind (best-hand ["HA" "DA" "CA" "HJ" "HT"])))
    (is (= :straight (best-hand ["HA" "DK" "HQ" "HJ" "HT"])))
    (is (= :straight (best-hand ["HA" "H2" "S3" "D4" "C5"])))
    (is (= :flush (best-hand ["HA" "HK" "H2" "H4" "HT"])))
    (is (= :full-house (best-hand ["HA" "DA" "CA" "HJ" "DJ"])))
    (is (= :four-of-a-kind (best-hand ["HA" "DA" "CA" "SA" "DJ"])))
    (is (= :straight-flush (best-hand ["HA" "HK" "HQ" "HJ" "HT"])))))
