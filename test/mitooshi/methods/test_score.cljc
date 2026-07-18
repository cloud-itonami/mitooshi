(ns mitooshi.methods.test-score
  "Cross-language oracle tests for mitooshi.methods.score — the proper-scoring engine.
  Ported 1:1 from the REAL Python test_score.py.

  score.cljc was already in use (forecast-quantile / horizon depend on it) but carried NO
  cljc test; this fills that gap AND completes the port (score-set was missing and is added
  in the same change). The closed-form oracle values (CRPS(N(0,1),0) ≈ 0.23370, ensemble-CRPS
  {-1,1}@0 = 0.5, …) come straight from the Python test, so the erf/score math is pinned across
  languages. The leak-free / G1 / G2 / G12 disciplines are exercised exactly as the Python does."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.score :as s]))

(defn- close? [x y tol] (< (Math/abs (- (double x) (double y))) tol))

;; ── CRPS ──────────────────────────────────────────────────────────────────────
(deftest crps-standard-normal-at-mean
  (is (close? (s/gaussian-crps 0.0 1.0 0.0) 0.23370 1e-4)))   ; 2φ(0) - 1/√π

(deftest crps-collapses-to-abs-error-as-sigma-to-zero
  (is (close? (s/gaussian-crps 10.0 1e-12 11.0) 1.0 1e-6)))

(deftest crps-nonnegative-and-scales-with-sigma
  (is (> (s/gaussian-crps 0.0 1.0 3.0) 0))
  (is (< (s/gaussian-crps 0.0 3.0 5.0) (s/gaussian-crps 0.0 1.0 5.0))))  ; honest uncertainty wins

(deftest crps-better-forecast-scores-lower
  (is (< (s/gaussian-crps 10.0 2.0 10.2) (s/gaussian-crps 4.0 2.0 10.2))))

;; ── log score / PIT ─────────────────────────────────────────────────────────────
(deftest logscore-minimized-near-mean
  (is (< (s/gaussian-logscore 10.0 2.0 10.0) (s/gaussian-logscore 10.0 2.0 16.0))))

(deftest pit-is-half-at-mean-and-monotone
  (is (close? (s/gaussian-pit 5.0 2.0 5.0) 0.5 1e-9))
  (is (< (s/gaussian-pit 5.0 2.0 1.0) 0.5 (s/gaussian-pit 5.0 2.0 9.0))))

;; ── pinball / quantile ──────────────────────────────────────────────────────────
(deftest pinball-zero-when-median-equals-outcome
  (is (= 0.0 (s/pinball-loss {0.5 7.0} 7.0))))

(deftest pinball-positive-and-asymmetric
  (let [q {0.1 2.0 0.5 5.0 0.9 8.0}]
    (is (> (s/pinball-loss q 5.0) 0))
    (is (> (s/pinball-loss q 9.0) (s/pinball-loss q 5.0)))))

(deftest quantile-pit-brackets
  (let [q {0.1 2.0 0.5 5.0 0.9 8.0}]
    (is (= 0.0 (s/quantile-pit q 1.0)))
    (is (= 1.0 (s/quantile-pit q 9.0)))
    (is (close? (s/quantile-pit q 5.0) 0.5 1e-9))))

;; ── brier / categorical ─────────────────────────────────────────────────────────
(deftest brier-perfect-confident-is-zero
  (is (= 0.0 (s/brier-score {"up" 1.0 "flat" 0.0 "down" 0.0} "up"))))

(deftest brier-worst-confident-wrong
  (is (close? (s/brier-score {"up" 1.0 "down" 0.0} "down") 2.0 1e-9)))

(deftest categorical-logscore-penalises-low-prob-truth
  (is (> (s/categorical-logscore {"a" 0.9 "b" 0.1} "b")
         (s/categorical-logscore {"a" 0.9 "b" 0.1} "a"))))

;; ── ensemble (energy-form CRPS) ───────────────────────────────────────────────
(deftest ensemble-crps-known-value
  (is (close? (s/ensemble-crps [-1.0 1.0] 0.0) 0.5 1e-9)))

(deftest ensemble-crps-reduces-to-abs-error-for-singleton
  (is (close? (s/ensemble-crps [3.0] 5.0) 2.0 1e-9)))

(deftest ensemble-crps-tight-correct-beats-vague
  (is (< (s/ensemble-crps [9.9 10.0 10.1] 10.0) (s/ensemble-crps [2.0 10.0 18.0] 10.0))))

(deftest ensemble-pit-fraction-at-or-below
  (is (= 0.5 (s/ensemble-pit [1.0 2.0 3.0 4.0] 2.5))))

(deftest score-pair-valid-ensemble
  (let [fc (s/->forecast "e" "ensemble" :info-as-of 100 :members [9.0 10.0 11.0])
        r (s/score-pair fc (s/->observation "o" :observed-at 101 :value 10.0))]
    (is (contains? r "crps"))
    (is (contains? r "pit"))
    (is (>= (get r "crps") 0))))

;; ── baselines + skill ───────────────────────────────────────────────────────────
(deftest climatology-and-persistence
  (let [[mu-c sd-c] (s/climatology-gaussian [4.0 5.0 6.0 5.0 4.0])
        [mu-p sd-p] (s/persistence-gaussian [4.0 5.0 6.0 5.0 4.0])]
    (is (close? mu-c 4.8 1e-9))
    (is (> sd-c 0))
    (is (= 4.0 mu-p))
    (is (> sd-p 0))))

(deftest skill-positive-when-model-beats-baseline
  (is (= 0.5 (s/skill-score 0.5 1.0)))
  (is (< (s/skill-score 1.5 1.0) 0)))

;; ── leak-free pair scorer (G5/G1/G2) ─────────────────────────────────────────────
(deftest score-pair-rejects-lookahead-leak
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5 LEAK"
                        (s/score-pair (s/->forecast "f" "gaussian" :info-as-of 100 :mean 10.0 :sd 2.0)
                                      (s/->observation "o" :observed-at 100 :value 10.0)))))

(deftest score-pair-rejects-point-assertion
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G1"
                        (s/score-pair (s/->forecast "f" "gaussian" :info-as-of 100 :mean 10.0 :sd 2.0 :point-asserted true)
                                      (s/->observation "o" :observed-at 101 :value 10.0)))))

(deftest score-pair-rejects-speculative-use
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2"
                        (s/score-pair (s/->forecast "f" "gaussian" :info-as-of 100 :mean 10.0 :sd 2.0 :use "trade")
                                      (s/->observation "o" :observed-at 101 :value 10.0)))))

(deftest score-pair-valid-gaussian
  (let [r (s/score-pair (s/->forecast "f" "gaussian" :info-as-of 100 :mean 10.0 :sd 2.0)
                        (s/->observation "o" :observed-at 101 :value 11.0))]
    (is (contains? r "crps"))
    (is (contains? r "log_score"))
    (is (contains? r "pit"))
    (is (> (get r "crps") 0))))

;; ── calibration ─────────────────────────────────────────────────────────────────
(deftest calibration-uniform-pit-low-deviation
  (let [pits (mapv (fn [i] (/ (+ i 0.5) 20)) (range 20))
        c (s/calibration-summary pits 10)]
    (is (close? (get c "pit_mean") 0.5 1e-9))
    (is (< (get c "deviation") 1e-9))))

(deftest calibration-clustered-pit-high-deviation
  (let [c (s/calibration-summary (vec (repeat 20 0.01)) 10)]
    (is (> (get c "deviation") 1.5))))

;; ── set-level skill (G12) ─────────────────────────────────────────────────────
(deftest score-set-marks-skilled-only-when-beating-baseline
  (let [pairs (mapv (fn [i] [(s/->forecast (str "f" i) "gaussian" :info-as-of (+ 100 i) :mean 10.0 :sd 2.0)
                             (s/->observation (str "o" i) :observed-at (+ 200 i) :value (+ 10.0 (* 0.1 i)))])
                    (range 5))
        res (s/score-set pairs (vec (repeat 5 {"crps" 5.0})))]
    (is (= 5 (get res "n")))
    (is (= true (get res "skilled")))
    (is (> (get res "skill") 0))
    (let [res2 (s/score-set pairs (vec (repeat 5 {"crps" 0.01})))]
      (is (= false (get res2 "skilled"))))))
