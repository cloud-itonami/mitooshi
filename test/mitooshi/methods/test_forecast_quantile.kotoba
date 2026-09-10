(ns mitooshi.methods.test-forecast-quantile
  "mitooshi 見通し — quantile-forecaster tests (ADR-2606051800). 1:1 Clojure port of
  methods/test_forecast_quantile.py, every assertion preserved, PLUS the constitutional
  gate tests made explicit and test-enforced:

    G1 distribution-only — the forecast is dist-kind=\"quantile\", point-asserted=false; a
       point-asserted forecast is RAISED on by score/score-pair (unrepresentable, 非終末論).
    G2 never-trades      — use=\":resilience\"; a speculative use is RAISED on by score-pair.
    G5 leak-free         — info-as-of < target; an obs not strictly after info-as-of RAISES.
    G12 anti-pseudoscience — :skilled is true ONLY when pinball beats the persistence baseline.

  Quantiles are monotone (q10 ≤ q50 ≤ q90)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [mitooshi.methods.forecast-quantile :as fq]
            [mitooshi.methods.score :as score]))

(defn- rising []
  (mapv (fn [t] [t (double (+ 10 (* 2 t)))]) (range 1 8)))   ; t=1..7

;; ── ported assertions ───────────────────────────────────────────────────
(deftest test-forecast-is-quantile-distribution-g1
  (let [fc (fq/forecast-next-quantile "s-x" (rising) 7)]
    (is (some? fc))
    (is (= "quantile" (:dist-kind fc)))
    (is (false? (:point-asserted fc)))                       ; G1
    (is (= ":resilience" (:use fc)))                         ; G2
    (is (= (set fq/default-levels) (set (keys (:quantiles fc)))))))

(deftest test-quantiles-are-monotone
  (let [fc (fq/forecast-next-quantile "s-x" (rising) 7)
        vals (mapv #(get (:quantiles fc) %) (sort (keys (:quantiles fc))))]
    (is (= vals (vec (sort vals))))))                         ; q10 <= q50 <= q90

(deftest test-leak-free-info-before-target-g5
  (let [fc (fq/forecast-next-quantile "s-x" (rising) 7)]
    (is (< (:info-as-of fc) 7))                               ; G5 — only prior history
    (let [s (fq/score-quantile fc 24.0 7)]                    ; obs strictly after info → no raise
      (is (contains? s "pinball")))))

(deftest test-no-prior-history-returns-none
  (is (nil? (fq/forecast-next-quantile "s-x" [[7 24.0]] 7))))

(deftest test-trail-scores-pinball-and-skill-g12
  (let [rows (mapv (fn [t] {":obs/series" "s-x" ":obs/observed-at" t
                            ":obs/value" (double (+ 10 (* 2 t)))}) (range 1 8))
        trail (fq/forecast-quantile-trail rows 7)]
    (is (= 1 (count trail)))
    (let [r (first trail)]
      (is (and (contains? r "pinball") (contains? r "baseline_pinball") (contains? r "skill")))
      (is (instance? Boolean (get r "skilled"))))))           ; G12

;; ── explicit gate enforcement (distribution-only / never-trades) ─────────
(deftest test-g1-point-asserted-forecast-raises
  ;; a deterministic single-future is unrepresentable: score-pair refuses it at the door.
  (let [fc (score/->forecast "fc.pt" "quantile" :info-as-of 6 :use ":resilience"
                             :point-asserted true :quantiles {0.1 20.0 0.5 22.0 0.9 24.0})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (score/score-pair fc (score/->observation "o" :observed-at 7 :value 24.0))))))

(deftest test-g2-speculative-use-raises
  ;; trade/position are NOT in the allowed-use set — never-trades is structural.
  (let [fc (score/->forecast "fc.tr" "quantile" :info-as-of 6 :use ":trade"
                             :quantiles {0.1 20.0 0.5 22.0 0.9 24.0})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (score/score-pair fc (score/->observation "o" :observed-at 7 :value 24.0))))))

(deftest test-g5-leak-raises-on-non-strict-obs
  (let [fc (fq/forecast-next-quantile "s-x" (rising) 7)]
    ;; obs observed-at == info-as-of (6) is NOT strictly after → look-ahead leak.
    (is (thrown? clojure.lang.ExceptionInfo
                 (fq/score-quantile fc 22.0 (:info-as-of fc))))))

#?(:clj
   (defn -main [& _] (run-tests 'mitooshi.methods.test-forecast-quantile)))
