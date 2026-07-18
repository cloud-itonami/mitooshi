(ns mitooshi.methods.test-promote
  "Tests for mitooshi backtest→promotion decision (methods/promote.cljc).
  1:1 port of methods/test_promote.py.

  The calibration_gate is a REFUSAL gate. These tests prove each gate fires on the scorecard:
  G12 (skill≤0 refused), G7 (miscalibrated refused — the real two-regime trail's outcome),
  G9 (unsigned / server-signed refused), and that a skilled+calibrated+member-signed model
  clears. The gate logic itself is the cell's review-promotion (single source of truth)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mitooshi.methods.promote :as p]))

(def member "did:web:etzhayyim.com:member:alice")

(defn- rows
  ([skill deviation] (rows skill deviation "persistence"))
  ([skill deviation method]
   [{":fc.score/method" (str ":" method)
     ":fc.score/mean-skill" skill
     ":fc.score/calibration-deviation" deviation}]))

;; The scorecard the Python test reads: methods/../data/persisted/...
(def ^:private scorecard-file
  (-> (io/file *file*)            ;; .../methods/test_promote.cljc
      (.getParentFile)            ;; .../methods
      (.getParentFile)           ;; .../mitooshi
      (io/file "data" "persisted" "chokepoint-backtest-scorecard.kotoba.edn")))

(deftest test-skilled-calibrated-signed-clears
  (let [d (first (p/decide-from-scorecard (rows 0.5 0.2) {:signed-by member}))]
    (is (= "cleared" (:phase d)))
    (is (true? (:promoted d)))))

(deftest test-unskilled-refused-g12
  (let [d (first (p/decide-from-scorecard (rows -0.1 0.2) {:signed-by member}))]
    (is (= "refused" (:phase d)))
    (is (str/includes? (:refusal d) "G12"))))

(deftest test-miscalibrated-refused-g7
  ;; skill is fine but deviation exceeds the ceiling → G7 refusal
  (let [d (first (p/decide-from-scorecard (rows 0.7 1.3) {:signed-by member}))]
    (is (= "refused" (:phase d)))
    (is (str/includes? (:refusal d) "G7"))))

(deftest test-unsigned-refused-g9-no-server-key
  (let [d (first (p/decide-from-scorecard (rows 0.5 0.2) {:signed-by ""}))]
    (is (= "refused" (:phase d)))
    (is (str/includes? (:refusal d) "G9"))))

(deftest test-server-signature-refused-g9
  (let [d (first (p/decide-from-scorecard (rows 0.5 0.2) {:signed-by "server:etzhayyim"}))]
    (is (= "refused" (:phase d)))
    (is (str/includes? (:refusal d) "G9"))))

(deftest test-decision-edn-records-server-held-key-false
  (let [edn (p/emit-decision-edn (p/decide-from-scorecard (rows 0.5 0.2) {:signed-by member}) member)]
    (is (str/includes? edn ":fc.promotion/server-held-key false"))
    (is (str/includes? edn ":fc.promotion/promoted true"))))

(deftest test-real-scorecard-is-refused-on-calibration
  ;; honest end-to-end: the real two-regime trail is SKILLED but MISCALIBRATED, so even
  ;; a member signature does NOT clear it — the gate working as designed.
  (when (.exists scorecard-file)
    (let [rows (p/load-edn scorecard-file)
          decisions (p/decide-from-scorecard rows {:signed-by member})]
      (is (seq decisions) "expected scorecard methods")
      (doseq [d decisions]
        (is (> (:skill d) 0))                                  ;; G12 satisfied (skilled)
        (is (= "refused" (:phase d)))
        (is (str/includes? (:refusal d) "G7"))))))            ;; but miscalibrated
