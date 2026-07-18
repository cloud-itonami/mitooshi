(ns mitooshi.methods.test-forecast
  "Tests for mitooshi baseline forecasting over the persisted trail (methods/forecast.cljc).
  1:1 port of methods/test_forecast.py. Proves observe→…→forecast is leak-free: forecasts are
  distributions (G1 point-asserted false), use only pre-target history (G5), and score with proper
  rules + skill vs climatology (G12). Synthetic upward-trend history for non-trivial skill."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mitooshi.methods.forecast :as fc]
            [mitooshi.methods.score :as score]
            [mitooshi.methods.analyze :as analyze]))

(defn- synthetic []
  (mapv (fn [t] {":obs/series" "s-x" ":obs/observed-at" t ":obs/value" (double (+ 10 (* 2 t)))})
        (range 1 8)))

(deftest test-series-histories-are-sorted
  (let [h (fc/series-histories (synthetic))]
    (is (contains? h "s-x"))
    (let [ts (mapv first (get h "s-x"))]
      (is (= ts (vec (sort ts)))))))

(deftest test-forecast-is-distribution-not-point
  (let [f (fc/forecast-next "s-x" (get (fc/series-histories (synthetic)) "s-x") 7)]
    (is (some? f))
    (is (= false (:point-asserted f)))         ; G1
    (is (= "gaussian" (:dist-kind f)))
    (is (= ":resilience" (:use f)))))

(deftest test-forecast-is-leak-free-info-as-of-before-target
  (let [f (fc/forecast-next "s-x" (get (fc/series-histories (synthetic)) "s-x") 7)]
    (is (< (:info-as-of f) 7))                  ; G5
    (let [s (score/score-pair f (score/->observation "o" :observed-at 7 :value 24.0))]
      (is (contains? s "crps")))))

(deftest test-forecast-next-returns-none-without-prior-history
  (is (nil? (fc/forecast-next "s-x" (get (fc/series-histories (synthetic)) "s-x") 1))))

(deftest test-persistence-beats-climatology-on-a-trend
  (let [rows (synthetic)
        pers (fc/forecast-trail rows 7 "persistence")
        clim (fc/forecast-trail rows 7 "climatology")
        pj (first (filter #(= "s-x" (get % "series")) pers))
        cj (first (filter #(= "s-x" (get % "series")) clim))]
    (is (< (get pj "crps") (get cj "crps")))    ; persistence tracks the trend
    (is (> (get pj "skill") 0))))               ; G12 — skilled vs climatology

(deftest test-emit-forecast-edn-marks-g1-and-g5
  (let [edn (fc/emit-forecast-edn (fc/forecast-trail (synthetic) 7 "climatology") 7 "climatology")]
    (is (str/includes? edn ":forecast/point-asserted false"))   ; G1
    (is (and (str/includes? edn "leak-free") (str/includes? edn "G10-gated")))))  ; G5 + G10

(deftest test-runs-over-real-persisted-trail-leak-free
  (let [trail (io/file "data/persisted/chokepoint-trail.kotoba.edn")]
    (when (.exists trail)
      (let [rows (analyze/read-edn (slurp trail))
            h (fc/series-histories rows)]
        (is (seq h))
        (let [target (apply max (mapcat (fn [[_ pairs]] (map first pairs)) h))
              fcs (fc/forecast-trail rows target "climatology")]
          (is (seq fcs))
          (doseq [r fcs] (is (< (:info-as-of (get r "forecast")) target))))))))
