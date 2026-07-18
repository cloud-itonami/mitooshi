(ns mitooshi.methods.test-analyze
  "mitooshi 見通し — backtest-analyzer tests (ADR-2606051800). 1:1 Clojure port of
  methods/test_analyze.py over the seed graph, PLUS the constitutional gate tests the task
  requires made explicit and test-enforced:

    G1 distribution-only / G2 never-trades — score/score-pair RAISES on a point-asserted
       forecast and on a speculative :forecast/use (trade/position/…); these futures are
       structurally unrepresentable. Asserted directly here.
    G5 leak-free — score/score-pair RAISES if the obs is not strictly after :info-as-of;
       and the seed backtest scores all 15 forecasts (none dropped, none leaked).
    G12 anti-pseudoscience — :skilled is true ONLY when a model beats a baseline on a
       proper score (the gaussian-skilled test).

  The four Python assertions over the seed are ported verbatim, plus reliability-diagram /
  reliability-datom assertions. forecast/score/persist/synthesize/promote-dependent tests
  in the sibling Python suites are out of scope here (those need unported sibling modules —
  promote.cljc is already ported separately; score.cljc IS ported and exercised here)."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.java.io :as io]
            [mitooshi.methods.analyze :as analyze]
            [mitooshi.methods.score :as score]))

(def seed
  (io/file (-> (io/resource "mitooshi/methods/test_analyze.cljc") io/file
               .getParentFile .getParentFile .getParentFile .getParentFile)
           "data" "seed-forecast-graph.kotoba.edn"))

(defn res [] (analyze/backtest (analyze/load-edn-file seed)))

(defn card [r model] (first (filter #(= (get % "model") model) (get r "cards"))))

;; ── ported assertions ───────────────────────────────────────────────────
(deftest test-seed-parses-four-models-all-distribution-kinds
  (let [r (res)]
    (is (= 4 (count (get r "cards"))))
    (is (= #{"gaussian" "quantile" "categorical" "ensemble"}
           (set (map #(get % "dist") (get r "cards")))))))

(deftest test-ensemble-model-scored-with-energy-crps
  (let [c (card (res) "m-e-edge")]
    (is (and (= "CRPS" (get c "metric")) (= "ensemble" (get c "dist")) (= 3 (get c "n"))))
    (is (and (<= 0 (get c "mean_primary")) (< (get c "mean_primary") 1.0)))
    (is (some? (get c "skill_vs_climatology")))))

(deftest test-gaussian-model-skilled-against-both-baselines
  (let [c (card (res) "m-ewma-drift")]
    (is (and (= "CRPS" (get c "metric")) (= 6 (get c "n"))))
    (is (> (get c "skill_vs_climatology") 0))
    (is (> (get c "skill_vs_persistence") 0))
    (is (true? (get c "skilled")))))

(deftest test-quantile-model-scored-with-pinball
  (let [c (card (res) "m-q-edge")]
    (is (and (= "pinball" (get c "metric")) (= 3 (get c "n"))))
    (is (and (< 0 (get c "mean_primary")) (< (get c "mean_primary") 1.0)))
    (is (nil? (get c "skill_vs_persistence")))))  ;; no gaussian-persistence baseline for quantile

(deftest test-categorical-model-scored-with-brier
  (let [c (card (res) "m-c-edge")]
    (is (and (= "Brier" (get c "metric")) (= 3 (get c "n"))))
    (is (and (<= 0 (get c "mean_primary")) (<= (get c "mean_primary") 2.0)))
    (is (some? (get c "skill_vs_climatology")))))  ;; beats/loses vs class-frequency climatology

(deftest test-gaussian-pit-mean-reflects-slight-positive-bias
  (let [c (card (res) "m-ewma-drift")]
    (is (and (< 0.4 (get c "pit_mean")) (< (get c "pit_mean") 0.7)))))

(deftest test-leak-free-all-forecasts-scored-none-dropped
  ;; smoke: backtest never raised a G5 leak → every forecast's obs is strictly after info.
  (let [r (res)]
    (is (= 15 (reduce + 0 (map #(get % "n") (get r "cards")))))))  ;; 6 + 3 + 3 + 3

(deftest test-reliability-diagram-has-a-section-per-model
  (let [r (res)
        md (analyze/render-reliability r)]
    (doseq [c (get r "cards")]
      (is (clojure.string/includes? md (get c "name"))))
    (is (and (clojure.string/includes? md "PIT mean")
             (clojure.string/includes? md "uniform ideal")))))

(deftest test-reliability-datoms-emit-calib-records
  (let [r (res)
        edn (analyze/render-reliability-datoms r)]
    (is (= (count (get r "cards"))
           (count (re-seq #":fc.calib/id" edn))))
    (is (and (clojure.string/includes? edn ":fc.calib/pit-mean")
             (clojure.string/includes? edn ":fc.calib/hist")))))

;; ── constitutional gate tests (G1 / G2 never-trades, G5 leak-free) ──────────
(deftest test-G1-point-asserted-forecast-is-unrepresentable
  (testing "G1: a deterministic single-future (point-asserted) is rejected at the door."
    (let [f (score/->forecast "f-pt" "gaussian" :info-as-of 1 :mean 1.0 :sd 1.0 :point-asserted true)
          o (score/->observation "o" :observed-at 2 :value 1.0)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G1" (score/score-pair f o))))))

(deftest test-G2-never-trades-speculative-use-is-unrepresentable
  (testing "G2: a speculative :forecast/use (trade/position/wager/speculation) is refused."
    (doseq [use ["trade" "position" "wager" "speculation" ":trade"]]
      (let [f (score/->forecast "f-sp" "gaussian" :info-as-of 1 :use use :mean 1.0 :sd 1.0)
            o (score/->observation "o" :observed-at 2 :value 1.0)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G2" (score/score-pair f o))
            (str "use " use " must be refused"))))))

(deftest test-G5-leak-is-refused
  (testing "G5: scoring an obs that is not strictly after :info-as-of would see the future."
    (let [f (score/->forecast "f" "gaussian" :info-as-of 5 :mean 1.0 :sd 1.0)]
      ;; equal timestamp leaks
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5"
                            (score/score-pair f (score/->observation "o" :observed-at 5 :value 1.0))))
      ;; earlier timestamp leaks
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"G5"
                            (score/score-pair f (score/->observation "o" :observed-at 4 :value 1.0))))
      ;; strictly-after is fine
      (is (map? (score/score-pair f (score/->observation "o" :observed-at 6 :value 1.0)))))))

;; ── byte-parity sentinel (the rendered seed report) ─────────────────────────
;; A representative slice of the EXACT bytes python3 analyze.py emits over the seed; if the
;; Clojure renderer drifts (float formatting, ordering, gate text), this fails immediately.
(deftest test-byte-identical-datoms-line
  (let [edn (analyze/render-reliability-datoms (res))]
    (is (clojure.string/includes?
         edn
         "{:fc.calib/id \"calib-m-ewma-drift\" :fc.calib/model \"m-ewma-drift\" :fc.calib/pit-mean 0.559174 :fc.calib/deviation 1.200000 :fc.calib/hist \"[0.0000 0.1667 0.0000 0.0000 0.1667 0.0000 0.3333 0.3333 0.0000 0.0000]\"}")))
  (let [edn (analyze/render-datoms (res))]
    (is (clojure.string/includes?
         edn
         "{:fc.score/id \"score-m-c-edge\" :fc.score/model \"m-c-edge\" :fc.score/metric \"Brier\" :fc.score/value 0.338333 :fc.score/pit 0.866667 :fc.score/skill 0.437994 :fc.model/skilled true :fc.score/derived true}"))))
