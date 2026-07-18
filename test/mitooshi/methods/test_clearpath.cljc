(ns mitooshi.methods.test-clearpath
  "End-to-end GREEN-path: a calibrated, skilled, member-signed model CLEARS the gate. 1:1 port of
  methods/test_clearpath.py. On a single-regime fixture calibrated persistence is skilled (G12) +
  calibrated (G7) → a member signature CLEARS promotion, refused unsigned (G9 no-server-key)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mitooshi.methods.forecast :as fc]
            [mitooshi.methods.promote :as promote]
            [mitooshi.methods.analyze :as analyze]))

(def fixture (io/file "data/fixtures/single-regime-trail.kotoba.edn"))
(def member "did:web:etzhayyim.com:member:alice")

(defn- scorecard-row [method]
  (let [rows (analyze/read-edn (slurp fixture))
        s (fc/backtest-calibrated rows method)]
    [{":fc.score/method" (str ":" method)
      ":fc.score/mean-skill" (get s "mean_skill")
      ":fc.score/calibration-deviation" (get-in s ["calibration" "deviation"])} s]))

(deftest test-fixture-persistence-is-skilled-and-calibrated
  (let [[_ s] (scorecard-row "persistence")]
    (is (> (get s "mean_skill") 0))                       ; G12
    (is (<= (get-in s ["calibration" "deviation"]) 0.4))))  ; G7

(deftest test-member-signed-persistence-clears
  (let [[row _] (scorecard-row "persistence")
        d (first (promote/decide-from-scorecard [row] {:signed-by member}))]
    (is (= "cleared" (:phase d))) (is (= true (:promoted d)))))

(deftest test-unsigned-persistence-refused-g9
  (let [[row _] (scorecard-row "persistence")
        d (first (promote/decide-from-scorecard [row] {:signed-by ""}))]
    (is (= "refused" (:phase d))) (is (str/includes? (:refusal d) "G9"))))

(deftest test-climatology-refused-on-calibration-g7
  (let [[row s] (scorecard-row "climatology")
        d (first (promote/decide-from-scorecard [row] {:signed-by member}))]
    (is (> (get s "mean_skill") 0))
    (is (= "refused" (:phase d))) (is (str/includes? (:refusal d) "G7"))))
