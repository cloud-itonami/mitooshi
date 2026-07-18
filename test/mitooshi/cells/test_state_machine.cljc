(ns mitooshi.cells.test-state-machine
  "State-machine tests for the mitooshi 見通し cells (R0). 1:1 port of the series_ingest/forecast_issue/
  calibration_gate/backtest_score portions of cells/test_state_machines.py (ADR-2606051800).
  (online_update has its own test_state_machine.cljc.) G4 source / G1·G2 forecast / G7·G9·G12 promotion
  / G5·G12 scoring — all REFUSAL gates; .solve() raises at R0."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [mitooshi.cells.series-ingest.state-machine :as si]
            [mitooshi.cells.forecast-issue.state-machine :as fi]
            [mitooshi.cells.calibration-gate.state-machine :as cg]
            [mitooshi.cells.backtest-score.state-machine :as bs]))

;; ── series_ingest (G4) ──
(defn- ingest [& {:keys [source-class] :or {source-class "public-broadcast"}}]
  (-> (si/transition-to-screened {"cell_state" {} "series_id" "s-malacca-transit" "source_class" source-class
                                  "source" "AISStream" "kind" ":transit-load"
                                  "obs" [{"observed_at" 200 "value" 3.0} {"observed_at" 100 "value" 2.0}]})
      si/transition-to-recorded))

(deftest test-series-ingest-public-source-records
  (let [cs (get (ingest) "cell_state")]
    (is (= "recorded" (get cs "phase")))
    (is (= 200 (get-in cs ["payload" "latestAt"]))) (is (= 3.0 (get-in cs ["payload" "latestValue"])))))

(deftest test-series-ingest-accepts-edn-keyword-source-class
  (is (= "recorded" (get-in (ingest :source-class ":gov-open-data") ["cell_state" "phase"]))))

(deftest test-series-ingest-refuses-proprietary-terminal
  (let [s (si/transition-to-screened {"cell_state" {} "series_id" "s-x" "source_class" "bloomberg-terminal"
                                      "source" "Bloomberg" "kind" ":price-index" "obs" []})]
    (is (= "refused" (get-in s ["cell_state" "phase"])))
    (is (str/includes? (get-in s ["cell_state" "refusal"]) "G4"))))

(deftest test-series-ingest-solve-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (si/solve {}))))

;; ── forecast_issue (G1/G2) ──
(deftest test-forecast-issue-valid-gaussian
  (let [cs (get (fi/issue {"cell_state" {} "forecast_id" "f1" "series_id" "s1" "dist_kind" "gaussian"
                           "use" "resilience" "info_as_of" 100 "horizon" 3 "mean" 3.2 "sd" 0.8}) "cell_state")]
    (is (= "issued" (get cs "phase")))
    (is (= 103 (get-in cs ["payload" "targetAt"]))) (is (= false (get-in cs ["payload" "pointAsserted"])))))

(deftest test-forecast-issue-refuses-point-assertion
  (let [cs (get (fi/issue {"cell_state" {} "forecast_id" "f" "dist_kind" "gaussian" "use" "planning"
                           "mean" 1.0 "sd" 1.0 "point_asserted" true}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G1"))))

(deftest test-forecast-issue-refuses-speculative-use
  (let [cs (get (fi/issue {"cell_state" {} "forecast_id" "f" "dist_kind" "gaussian" "use" "trade" "mean" 1.0 "sd" 1.0}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G2"))))

(deftest test-forecast-issue-refuses-degenerate-gaussian
  (is (= "refused" (get-in (fi/issue {"cell_state" {} "forecast_id" "f" "dist_kind" "gaussian" "use" "nowcast" "mean" 1.0 "sd" 0.0}) ["cell_state" "phase"]))))

(deftest test-forecast-issue-refuses-unnormalized-categorical
  (is (= "refused" (get-in (fi/issue {"cell_state" {} "forecast_id" "f" "dist_kind" "categorical" "use" "early-warning"
                                      "probs" {"up" 0.5 "down" 0.2}}) ["cell_state" "phase"]))))

;; ── calibration_gate (G7/G9/G12) ──
(defn- promote [& {:keys [skill deviation signed-by point] :or {skill 0.3 deviation 0.1 signed-by "did:web:etzhayyim.com:member:op" point false}}]
  (cg/review-promotion {"cell_state" {} "model_id" "m1" "to_version" 2 "skill" skill "deviation" deviation
                        "signed_by" signed-by "point_asserted_any" point}))

(deftest test-calibration-gate-clears-skilled-calibrated-signed
  (let [cs (get (promote) "cell_state")]
    (is (= "cleared" (get cs "phase")))
    (is (= true (get-in cs ["payload" "promoted"]))) (is (= false (get-in cs ["payload" "serverHeldKey"])))))

(deftest test-calibration-gate-refuses-unskilled
  (let [cs (get (promote :skill -0.1) "cell_state")] (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G12"))))
(deftest test-calibration-gate-refuses-miscalibrated
  (let [cs (get (promote :deviation 0.9) "cell_state")] (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G7"))))
(deftest test-calibration-gate-refuses-server-signature
  (let [cs (get (promote :signed-by "server-key-1") "cell_state")] (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G9"))))
(deftest test-calibration-gate-refuses-unsigned
  (let [cs (get (promote :signed-by "") "cell_state")] (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G9"))))
(deftest test-calibration-gate-refuses-point-assertion
  (let [cs (get (promote :point true) "cell_state")] (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G1"))))

;; ── backtest_score (G5/G12) ──
(defn- pair [& {:keys [fid info mean sd at y point use] :or {fid "f" info 100 mean 10.0 sd 1.0 at 101 y 10.0 point false use "resilience"}}]
  {"forecast" {"forecastId" fid "distKind" "gaussian" "infoAsOf" info "mean" mean "sd" sd "pointAsserted" point "use" use}
   "obs" {"obsId" "o" "observedAt" at "value" y}})

(deftest test-backtest-score-scores-a-clean-batch
  (let [cs (get (bs/score-batch {"cell_state" {} "model_id" "m" "baseline_primary" 1.0
                                 "pairs" [(pair :y 10.2) (pair :fid "g" :y 9.8)]}) "cell_state")]
    (is (= "scored" (get cs "phase")))
    (is (= 2 (get-in cs ["payload" "n"]))) (is (= "crps" (get-in cs ["payload" "metric"])))
    (is (= true (get-in cs ["payload" "skilled"])))))

(deftest test-backtest-score-refuses-leak
  (let [cs (get (bs/score-batch {"cell_state" {} "model_id" "m" "pairs" [(pair :info 100 :at 100)]}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G5 LEAK"))))

(deftest test-backtest-score-refuses-point-assertion
  (let [cs (get (bs/score-batch {"cell_state" {} "model_id" "m" "pairs" [(pair :point true)]}) "cell_state")]
    (is (= "refused" (get cs "phase"))) (is (str/includes? (get cs "refusal") "G1"))))

(deftest test-backtest-score-refuses-empty-batch
  (is (= "refused" (get-in (bs/score-batch {"cell_state" {} "model_id" "m" "pairs" []}) ["cell_state" "phase"]))))

(deftest test-remaining-cells-solve-raises
  (doseq [solve [fi/solve cg/solve bs/solve]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (solve {})))))
