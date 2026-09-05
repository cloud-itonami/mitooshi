(ns mitooshi.methods.test-influence
  "mitooshi 見通し — research-influence observation tests (deterministic fixtures).

  Gates exercised in code:
    G4  never-a-person — a person subject is refused; the subject enum has no person.
    PR  provenance — a metric without sourceClass/sourceRef is refused; a source that
        could not answer is flagged :missing and contributes NOTHING to any count
        (missingness is preserved, never coerced to zero).
    WINDOW — start < end enforced; the refresh trail records the window it measured.
    COVERAGE — uncertainty/missingness flags ride on the record, not folded into a score.
    NC  no causal / ranking language in the derived observation (refused, not stripped).
    AP  refresh history is append-only and idempotent (非終末論).
    AU  Hyakka proposal is audit-only; readback verifies the stored copy byte-for-byte,
        the interpretation booleans, and that the refresh history never shrank."
  (:require [clojure.test :refer [deftest is run-tests testing]]
            [clojure.string :as str]
            [mitooshi.methods.influence :as influence]))

(def ^:private SUBJECT {"kind" "work" "id" "doi:10.0000/example.2026"})
(def ^:private WINDOW {"start" 1756684800 "end" 1759276800}) ; 2026-09-01..2026-10-01, half-open

(def ^:private METRICS
  [{"kind" "citation-count" "value" 12
    "sourceClass" "primary-disclosure" "sourceRef" "crossref:works/doi:10.0000/example.2026"
    "asOf" 1759276799}
   {"kind" "mention-count" "value" 0
    "sourceClass" "open-commons" "sourceRef" "hyakka:mentions/2026-09"
    "asOf" 1759276799}
   ;; a source that could not answer — flagged missing, NOT zeroed
   {"kind" "download-count" "value" 0
    "sourceClass" "gov-open-data" "sourceRef" "japan-go-jp/usage-stats"
    "asOf" 1759276799 "missing" true}
   {"kind" "funding-record-count" "value" 1
    "sourceClass" "gov-open-data" "sourceRef" "kaken:grants/2026"
    "asOf" 1759276799}])

(defn- refusal-of
  [thunk]
  (try (thunk) nil
       (catch #?(:clj Exception :cljs js/Error) e
         {:msg (ex-message e)})))

(defn- valid-observation
  []
  (influence/build-observation
    {:observation-id "influence.obs/example.2026-w1"
     :subject SUBJECT :window WINDOW
     :metrics METRICS
     :coverage-flags ["non-response-bias-possible"]}))

;; ── construction ──────────────────────────────────────────────────────────────

(deftest test-builds-a-valid-record
  (let [r (valid-observation)]
    (is (= "influence.obs/example.2026-w1" (get r "observationId")))
    (is (= influence/METHOD-VERSION (get r "methodVersion")))
    (is (= WINDOW (get r "window")))
    (is (= 4 (count (get r "metrics"))))
    ;; the interpretation constraints ride on EVERY record, all true
    (is (= influence/INTERPRETATION (get r "interpretation")))
    (is (every? true? (vals (get r "interpretation"))))
    ;; the missing source is flagged, never zeroed into the aggregate
    (is (some #{"source-unavailable"} (get-in r ["coverage" "flags"])))
    (is (some #{"non-response-bias-possible"} (get-in r ["coverage" "flags"])))
    (is (= 3 (count (filter #(not (true? (get % "missing"))) (get r "metrics")))))
    (is (str/includes? (get-in r ["derived" "observationText"]) "欠測 1 件"))
    (is (str/includes? (get-in r ["derived" "observationText"]) "因果を主張するものではない"))
    (is (= "descriptive-aggregate" (get-in r ["derived" "kind"])))))

(deftest test-deterministic
  (is (= (valid-observation) (valid-observation))))

;; ── G4: never a person ────────────────────────────────────────────────────────

(deftest test-person-subject-refused
  (is (re-find #"G4/SUBJECT"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x"
                           :subject {"kind" "person" "id" "orcid:0000-0000-0000-0000"}
                           :window WINDOW :metrics []}))))))

;; ── WINDOW ────────────────────────────────────────────────────────────────────

(deftest test-window-must-be-ordered
  (is (re-find #"WINDOW"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x" :subject SUBJECT
                           :window {"start" 10 "end" 10} :metrics []})))))
  (is (re-find #"WINDOW"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x" :subject SUBJECT
                           :window {"start" 20 "end" 10} :metrics []}))))))

;; ── PR: provenance mandatory; missing ≠ zero ─────────────────────────────────

(deftest test-metric-without-provenance-refused
  (is (re-find #"PR/METRIC"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x" :subject SUBJECT :window WINDOW
                           :metrics [{"kind" "citation-count" "value" 3
                                      "sourceClass" "open-commons"
                                      "sourceRef" "" "asOf" 100}]})))))
  (is (re-find #"PR/METRIC"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x" :subject SUBJECT :window WINDOW
                           :metrics [{"kind" "citation-count" "value" 3
                                      "sourceClass" "proprietary-terminal"
                                      "sourceRef" "somewhere" "asOf" 100}]}))))))

(deftest test-missing-metric-preserved-as-missing
  (let [r (influence/build-observation
            {:observation-id "x" :subject SUBJECT :window WINDOW
             :metrics [{"kind" "citation-count" "value" 0
                        "sourceClass" "open-commons" "sourceRef" "s" "asOf" 1
                        "missing" true}]
             :coverage-flags ["source-unavailable"]})]
    (is (= "source-unavailable" (first (get-in r ["coverage" "flags"]))))
    (is (true? (get (first (get r "metrics")) "missing")))
    ;; a missing metric must NEVER be silently read as a zero
    (is (str/includes? (get-in r ["derived" "observationText"]) "0 とは置かない"))))

;; ── NC: no causal / ranking language ─────────────────────────────────────────

(deftest test-causal-or-ranking-notes-refused
  (doseq [note ["paper X ranks above paper Y"
                "citations cause better outcomes"
                "this venue is the best"
                "funding drives output"]]
    (is (re-find #"NC/LANGUAGE"
                 (:msg (refusal-of
                         #(influence/build-observation
                            {:observation-id "x" :subject SUBJECT :window WINDOW
                             :metrics [] :notes [note]})))))))

;; ── AP: append-only, idempotent refresh ──────────────────────────────────────

(deftest test-refresh-appends-and-never-overwrites
  (let [[r1 a1] (influence/refresh (valid-observation) 1759276800)
        [r2 a2] (influence/refresh r1 1759276900)
        [r3 a3] (influence/refresh r2 1759276900)] ; same inputs hash → idempotent
    (is a1)
    (is a2)
    (is (not a3))
    (is (= 1 (count (get r1 "refreshHistory"))))
    (is (= 2 (count (get r2 "refreshHistory"))))
    (is (= 2 (count (get r3 "refreshHistory"))))
    ;; entries are immutable and ordered (trail, 非終末論)
    (is (= (get r1 "refreshHistory") (subvec (get r2 "refreshHistory") 0 1)))
    (is (every? #(= influence/METHOD-VERSION (get % "methodVersion"))
                (get r2 "refreshHistory")))
    (is (every? #(re-matches #"[0-9a-f]{8}" (get % "inputsHash"))
                (get r2 "refreshHistory")))))

(deftest test-inputs-hash-is-deterministic-and-input-sensitive
  (let [h1 (influence/inputs-hash SUBJECT WINDOW METRICS ["non-response-bias-possible"])
        h2 (influence/inputs-hash SUBJECT WINDOW METRICS ["non-response-bias-possible"])
        h3 (influence/inputs-hash SUBJECT WINDOW (rest METRICS) [])]
    (is (= h1 h2))                             ; reproducible
    (is (not= h1 h3))                          ; sensitive to inputs
    (is (re-matches #"[0-9a-f]{8}" h1))))

;; ── AU: audit-only Hyakka proposal + readback ────────────────────────────────

(deftest test-proposal-is-audit-only-and-readback-verifies
  (let [record (valid-observation)
        [record2 _] (influence/refresh record 1759276800)
        proposal (influence/hyakka-proposal record2)
        stored proposal ; happy path: Hyakka stored exactly what was proposed
        rb (influence/hyakka-readback proposal stored)]
    (is (= "research-influence-observation-proposal" (get proposal "kind")))
    (is (true? (get proposal "noAction")))
    (is (= "audit-only" (get proposal "requestedAction")))
    ;; no action is requested: contact/funding/hiring/venue/publication are named out of scope
    (is (= #{"contact" "funding" "sponsorship" "hiring" "venue-decision"
             "publication-decision" "policy-decision"}
           (set (get proposal "outOfScope"))))
    (is (true? (get rb "matches")))
    (is (empty? (get rb "diffs")))))

(deftest test-readback-detects-tampering
  (let [[record _] (influence/refresh (valid-observation) 1759276800)
        proposal (influence/hyakka-proposal record)
        stored-shrunk (assoc-in proposal ["record" "refreshHistory"] [])
        rb1 (influence/hyakka-readback proposal stored-shrunk)
        stored-tampered (-> proposal
                            (assoc-in ["record" "interpretation" "noRanking"] false)
                            (assoc-in ["record" "metrics"] [{"kind" "citation-count" "value" 99
                                                             "sourceClass" "open-commons"
                                                             "sourceRef" "s" "asOf" 1}]))
        rb2 (influence/hyakka-readback proposal stored-tampered)]
    (is (false? (get rb1 "matches")))
    (is (some #{"refresh-history-shrunk"} (get rb1 "diffs")))
    (is (false? (get rb2 "matches")))
    (is (some #{"record-mismatch"} (get rb2 "diffs")))
    (is (some #{"interpretation/noRanking-violated"} (get rb2 "diffs")))))

(deftest test-proposal-refuses-a-record-without-interpretation
  (is (re-find #"AU/PROPOSAL"
               (:msg (refusal-of
                       #(influence/hyakka-proposal
                          {"interpretation" {"noRanking" false}}))))))

;; ── coverage flags are validated, not invented ───────────────────────────────

(deftest test-unknown-coverage-flag-refused
  (is (re-find #"COVERAGE"
               (:msg (refusal-of
                       #(influence/build-observation
                          {:observation-id "x" :subject SUBJECT :window WINDOW
                           :metrics [] :coverage-flags ["looks-fine"]}))))))
