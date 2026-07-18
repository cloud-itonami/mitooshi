(ns mitooshi.methods.test-horizon
  "mitooshi 見通し — multi-horizon skill-decay tests (ADR-2606051800). 1:1 Clojure port of
  methods/test_horizon.py, every assertion preserved.

  Gates upheld by construction (the AR(1) forecaster is a distribution scored leak-free):
    G1 distribution-only / G2 never-trades — every forecast is a gaussian, point-asserted
       false, use=\":resilience\"; score/score-pair refuses a point-assertion or a
       speculative use (structurally unrepresentable).
    G5 leak-free — each origin's obs is strictly after info-as-of; many origins per horizon.
    G12 skill-honest — skill_vs_clim is pinball/CRPS skill vs the climatology baseline, and
       it DECAYS with horizon (never a flat-skill crystal ball, 非終末論)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [mitooshi.methods.horizon :as horizon]))

(deftest test-path-is-deterministic-and-mean-reverting
  (let [a (horizon/build-path 50)
        b (horizon/build-path 50)]
    (is (= a b))                                     ; reproducible (no RNG)
    (is (every? #(and (< 5.0 %) (< % 15.0)) a))))    ; mean-reverting near MU=10

(deftest test-short-horizon-has-positive-skill
  (let [rows (horizon/horizon-skill)
        h1 (first (filter #(= 1 (get % "h")) rows))]
    (is (> (get h1 "skill_vs_clim") 0.1))))          ; clearly beats climatology at h=1

(deftest test-skill-decays-with-horizon
  (let [rows (horizon/horizon-skill)
        first* (first rows)
        last* (peek rows)]
    (is (> (get first* "skill_vs_clim") (get last* "skill_vs_clim")))   ; decays
    (is (< (get last* "skill_vs_clim") 0.1))))                          ; → ≈ climatology

(deftest test-crps-grows-with-horizon
  (let [rows (horizon/horizon-skill)]
    (is (> (get (peek rows) "mean_crps") (get (first rows) "mean_crps")))))

(deftest test-leak-free-every-origin-scored
  (let [rows (horizon/horizon-skill)]
    (is (every? #(> (get % "n") 10) rows))))         ; many leak-checked origins per horizon

(deftest test-render-md-has-a-row-per-horizon
  (let [rows (horizon/horizon-skill)
        md (horizon/render-md rows)]
    (doseq [r rows]
      (is (clojure.string/includes? md (str "| " (get r "h") " |"))))
    (is (clojure.string/includes? md "skill vs clim"))))

#?(:clj
   (defn -main [& _] (run-tests 'mitooshi.methods.test-horizon)))
