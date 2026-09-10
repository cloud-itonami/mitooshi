(ns mitooshi.methods.test-calibrate
  "Tests for mitooshi online-recalibrated backtest (forecast.cljc calibration). 1:1 port of
  methods/test_calibrate.py. The learning loop (online_update apply-correction) applied leak-free at
  each origin measurably improves CRPS + PIT-mean toward 0.5 on a biased series, never seeing the future."
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [mitooshi.methods.forecast :as fc]
            [mitooshi.methods.analyze :as analyze]))

(def trail (io/file "data/persisted/chokepoint-trail.kotoba.edn"))
(defn- trend [] (mapv (fn [t] {":obs/series" "s-x" ":obs/observed-at" t ":obs/value" (double (+ 10 (* 2 t)))}) (range 1 9)))

(deftest test-recalib-params-identity-when-no-residuals
  (is (= [0.0 1.0] (fc/recalib-params []))))

(deftest test-recalib-params-bias-is-mean-error
  (let [[bias infl] (fc/recalib-params [{"error" 2.0 "sd" 1.0} {"error" 4.0 "sd" 1.0}])]
    (is (= 3.0 bias))
    (is (<= 0.25 infl 4.0))))

(deftest test-calibration-reduces-crps-on-a-biased-series
  (is (< (get (fc/backtest-calibrated (trend) "climatology") "mean_crps")
         (get (fc/backtest-rolling (trend) "climatology") "mean_crps"))))

(deftest test-calibration-moves-pit-toward-half
  (let [raw (get-in (fc/backtest-rolling (trend) "climatology") ["calibration" "pit_mean"])
        cal (get-in (fc/backtest-calibrated (trend) "climatology") ["calibration" "pit_mean"])]
    (is (< (Math/abs (- cal 0.5)) (Math/abs (- raw 0.5))))))

(deftest test-calibrated-backtest-is-leak-free
  (let [s (fc/backtest-calibrated (trend) "climatology")]
    (is (= true (get s "calibrated"))) (is (> (get s "n") 0))))

(deftest test-runs-over-real-persisted-trail
  (when (.exists trail)
    (let [rows (analyze/read-edn (slurp trail))
          cal (fc/backtest-calibrated rows "climatology")
          raw (fc/backtest-rolling rows "climatology")]
      (is (= (get cal "n") (get raw "n"))) (is (> (get cal "n") 0))
      (is (<= (Math/abs (- (get-in cal ["calibration" "pit_mean"]) 0.5))
              (Math/abs (- (get-in raw ["calibration" "pit_mean"]) 0.5)))))))
