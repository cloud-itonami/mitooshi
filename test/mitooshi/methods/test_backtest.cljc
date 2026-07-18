(ns mitooshi.methods.test-backtest
  "Tests for mitooshi rolling-origin backtest (forecast.cljc). 1:1 port of methods/test_backtest.py.
  Leak-free per origin, persistence beats climatology on a trend across origins, scorecard EDN carries
  the G5/G12/G10 markers + per-method aggregate skill."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mitooshi.methods.forecast :as fc]
            [mitooshi.methods.analyze :as analyze]))

(def trail (io/file "data/persisted/chokepoint-trail.kotoba.edn"))
(defn- trend [] (mapv (fn [t] {":obs/series" "s-x" ":obs/observed-at" t ":obs/value" (double (+ 10 (* 2 t)))}) (range 1 8)))
(defn- noisy-flat []
  (mapv (fn [[i v]] {":obs/series" "s-y" ":obs/observed-at" (inc i) ":obs/value" v})
        (map-indexed vector [5.0 5.4 4.7 5.1 4.9 5.2 5.0])))

(deftest test-backtest-scores-every-origin-after-first
  (let [s (fc/backtest-rolling (trend) "persistence")]
    (is (= 6 (count (get s "per_origin"))))
    (is (every? #(>= (get % "target_at") 2) (get s "per_origin")))))

(deftest test-backtest-is-leak-free-no-raise
  (let [s (fc/backtest-rolling (trend) "climatology")]
    (is (> (get s "n") 0)) (is (some? (get s "mean_crps")))))

(deftest test-persistence-beats-climatology-across-origins-on-trend
  (let [comp (fc/compare-methods (trend))]
    (is (< (get-in comp ["persistence" "mean_crps"]) (get-in comp ["climatology" "mean_crps"])))
    (is (> (get-in comp ["persistence" "mean_skill"]) 0))))

(deftest test-calibration-summary-present
  (let [s (fc/backtest-rolling (noisy-flat) "climatology")
        cal (get s "calibration")]
    (is (<= 0.0 (get cal "pit_mean") 1.0)) (is (= (get cal "n") (get s "n")))))

(deftest test-scorecard-edn-marks-invariants-and-methods
  (let [edn (fc/emit-scorecard-edn (fc/compare-methods (trend)))]
    (is (and (str/includes? edn ":fc.score/method :persistence") (str/includes? edn ":fc.score/method :climatology")))
    (is (and (str/includes? edn "leak-free") (str/includes? edn "G10-gated") (str/includes? edn ":fc.score/mean-skill")))))

(deftest test-runs-over-real-persisted-trail
  (when (.exists trail)
    (let [comp (fc/compare-methods (analyze/read-edn (slurp trail)))]
      (is (> (get-in comp ["climatology" "n"]) 0)) (is (> (get-in comp ["persistence" "n"]) 0)))))
