(ns mitooshi.methods.influence
  "mitooshi 見通し — research-influence OBSERVATION method (bounded, offline, stdlib-only).

  One narrow contract: turn raw, provenance-carrying counts about a PUBLIC ARTIFACT
  (a work, dataset, topic, or venue) into ONE reproducible, provenance-preserving
  descriptive observation — with its measurement window, method version,
  uncertainty/coverage flags, append-only refresh history, and a Hyakka
  audit-only proposal + readback.

  What this method deliberately does NOT do (refusals are in code, not prose):
    G4  never a person — the subject enum has no 'person' member; a person-keyed
        observation, or any ranking of researchers, is refused at the door.
    NC  no causal / evaluative language — notes containing causal or ranking
        vocabulary (cause, rank, outperform, better, …) are refused; the derived
        observation is descriptive-aggregate ONLY.
    PR  provenance mandatory — a metric without sourceClass + sourceRef is refused;
        an unanswerable source is flagged :missing, never silently zeroed.
    AP  append-only history — a refresh appends {refreshedAt methodVersion inputsHash};
        nothing is ever overwritten or removed (非終末論).
    AU  audit-only — the Hyakka proposal requests no action; contact, funding,
        sponsorship, hiring, venue, and publication decisions are out of scope.

  House style: string-keyed maps (lexicon property names), pure functions,
  file I/O only at #?(:clj) edges, no deps. The inputsHash is a NON-cryptographic
  fnv1a-32 content fingerprint used for dedup/reproducibility only."
  (:require [clojure.string :as str]))

(def METHOD-VERSION "1.0.0")

;; G4 — subject kinds. 'person' is intentionally absent.
(def SUBJECT-KINDS ["work" "dataset" "topic" "venue"])
;; metric kinds this bounded contract understands; anything else is out of scope.
(def METRIC-KINDS ["citation-count" "mention-count" "download-count" "funding-record-count"])
;; provenance classes (same vocabulary as seriesObservation.edn — primary-public only)
(def SOURCE-CLASSES ["public-broadcast" "primary-disclosure" "open-commons" "gov-open-data" "member-principal"])
;; uncertainty / missingness flags
(def COVERAGE-FLAGS ["source-unavailable" "partial-window" "retraction-present" "denominator-unknown" "non-response-bias-possible"])

;; the reading constraints restated in EVERY emitted record
(def INTERPRETATION
  {"citationNotSupport"      true
   "fundingNotEndorsement"   true
   "attentionNotImpact"      true
   "correlationNotCausation" true
   "noRanking"               true})

;; NC — vocabulary that would turn a count into a causal/evaluative claim.
(def ^:private BANNED-WORDS
  ["cause" "caused" "causes" "causal" "because of" "due to" "drives" "driven by"
   "rank" "ranked" "ranking" "outperform" "beats" "better than" "worse than"
   "top " "best" "worst" "leader" "leading" "superior" "impact of" "proves"])

(def ^:private hex "0123456789abcdef")

(defn fnv1a-32-hex
  "Deterministic non-cryptographic 32-bit FNV-1a over a string, as 8-char lowercase hex.
  32-bit by construction: every step is masked to 32 bits, so clj (64-bit bit-ops) and
  cljs (32-bit ToInt32 bit-ops) produce IDENTICAL output. Pure, no deps."
  [s]
  (let [basis 0x811c9dc5
        prime 0x01000193
        p0 (bit-and prime 0xFFFF)                 ; 0x0193
        p1 (bit-and (unsigned-bit-shift-right prime 16) 0xFFFF) ; 0x0100
        ;; h = (h * p) mod 2^32 via two 16-bit lanes (fits in doubles / longs exactly)
        mul (fn [h x]
              (let [h0 (bit-and h 0xFFFF)
                    h1 (bit-and (unsigned-bit-shift-right h 16) 0xFFFF)
                    c0 (+ (* h0 p0) x)
                    c1 (+ (* h1 p0) (* h0 p1) (unsigned-bit-shift-right c0 16))]
                (bit-or (bit-shift-left (bit-and c1 0xFFFF) 16)
                        (bit-and c0 0xFFFF))))
        step (fn [acc ch] (mul acc (bit-and (int ch) 0xFF)))
        acc (reduce step basis (map int s))]
    (apply str (map #(nth hex (bit-and (unsigned-bit-shift-right acc (* 4 %)) 0xF))
                    (range 7 -1 -1)))))

(defn inputs-hash
  "Deterministic fingerprint over the observation inputs (subject/window/metrics/flags).
  Order-sensitive and canonical: pr-str of a sorted-key map."
  [subject window metrics coverage-flags]
  (fnv1a-32-hex
    (pr-str (sorted-map
              "subject" subject
              "window" window
              "metrics" metrics
              "coverageFlags" coverage-flags
              "methodVersion" METHOD-VERSION))))

(defn- refuse
  [code msg]
  (throw (ex-info (str code ": " msg) {:refusal code})))

(defn validate-subject
  "G4 — the subject must be a public artifact kind, never a person."
  [subject]
  (when-not (and (map? subject) (string? (get subject "id")))
    (refuse "G4/SUBJECT" "subject must be a map with a string id"))
  (let [k (get subject "kind")]
    (when-not (some #(= % k) SUBJECT-KINDS)
      (refuse "G4/SUBJECT" (str "subject kind " (pr-str k) " not in " (pr-str SUBJECT-KINDS)
                                " — an observation is never keyed by a person"))))
  subject)

(defn validate-window
  "WINDOW — start < end, both integer epochs. A half-open window is the contract."
  [window]
  (when-not (and (map? window)
                 (int? (get window "start"))
                 (int? (get window "end"))
                 (< (get window "start") (get window "end")))
    (refuse "WINDOW" "window must be {start end} integer epochs with start < end"))
  window)

(defn validate-metric
  "PR — every metric carries kind/value/sourceClass/sourceRef/asOf; missing is a flag
  (the metric is then absent from the count, never coerced to zero)."
  [m]
  (when-not (map? m)
    (refuse "PR/METRIC" "metric must be a map"))
  (let [k (get m "kind")]
    (when-not (some #(= % k) METRIC-KINDS)
      (refuse "PR/METRIC" (str "metric kind " (pr-str k) " not in " (pr-str METRIC-KINDS)))))
  (when-not (some #(= % (get m "sourceClass")) SOURCE-CLASSES)
    (refuse "PR/METRIC" (str "metric sourceClass " (pr-str (get m "sourceClass"))
                             " not in " (pr-str SOURCE-CLASSES) " — provenance class is mandatory")))
  (when (str/blank? (get m "sourceRef"))
    (refuse "PR/METRIC" "metric sourceRef is mandatory — a count without provenance is refused"))
  (when-not (int? (get m "asOf"))
    (refuse "PR/METRIC" "metric asOf must be an integer epoch"))
  (let [v (get m "value")]
    (when (and (not (true? (get m "missing"))) (not (and (int? v) (>= v 0))))
      (refuse "PR/METRIC" "metric value must be an integer >= 0 (a missing source is flagged, not zeroed)")))
  m)

(defn validate-notes
  "NC — notes and the observation text may not carry causal or ranking vocabulary."
  [notes text]
  (let [hay (str/lower-case (str text " " (str/join " " notes)))
        hit (some #(when (str/includes? hay %) %) BANNED-WORDS)]
    (when hit
      (refuse "NC/LANGUAGE" (str "descriptive observations may not use causal/ranking vocabulary, found: "
                                 (pr-str hit))))
    notes))

(defn build-observation
  "Build ONE validated research-influence observation record (pure). Refuses a person
  subject (G4), a bad window, a metric without provenance (PR), and any causal or
  ranking vocabulary in notes (NC). The derived section is descriptive-aggregate only:
  what was counted, over which window, with which uncertainty flags."
  [{:keys [observation-id subject window metrics coverage-flags notes]
    :or {coverage-flags [] notes []}}]
  (validate-subject subject)
  (validate-window window)
  (doseq [m metrics] (validate-metric m))
  (doseq [f coverage-flags]
    (when-not (some #(= % f) COVERAGE-FLAGS)
      (refuse "COVERAGE" (str "coverage flag " (pr-str f) " not in " (pr-str COVERAGE-FLAGS)))))
  (when-not (string? observation-id)
    (refuse "ID" "observationId is required"))
  (let [missing (filter #(true? (get % "missing")) metrics)
        present (remove #(true? (get % "missing")) metrics)
        flags (vec (distinct (concat
                               (when (seq missing) ["source-unavailable"])
                               (when (empty? metrics) ["denominator-unknown"])
                               coverage-flags)))
        text (str "観測(記述的): "
                  (get subject "kind") " " (get subject "id")
                  " の窓 [" (get window "start") ", " (get window "end")
                  ") における計数 " (count present) " 件"
                  (when (seq missing) (str "(欠測 " (count missing) " 件 — 0 とは置かない)"))
                  "。これは引用・注目・資金記録の記述的集計であり、研究の質・影響・"
                  "因果を主張するものではない。順位付けはしない。")
        _ (validate-notes notes text)
        derived {"kind" "descriptive-aggregate"
                 "observationText" text
                 "notes" (vec (distinct notes))}
        record {"observationId" observation-id
                "subject" (validate-subject subject)
                "window" window
                "methodVersion" METHOD-VERSION
                "metrics" (vec metrics)
                "coverage" {"flags" flags}
                "interpretation" INTERPRETATION
                "derived" derived
                "refreshHistory" []}]
    record))

(defn refresh
  "AP — append one refresh event {refreshedAt methodVersion inputsHash} to the record's
  refreshHistory. Idempotent: the same (refreshedAt, inputsHash) pair is NOT re-added;
  nothing existing is ever mutated or removed (非終末論). Returns [record' appended?]."
  [record refreshed-at]
  (when-not (int? refreshed-at)
    (refuse "REFRESH" "refreshedAt must be an integer epoch"))
  (let [h (inputs-hash (get record "subject") (get record "window")
                       (get record "metrics") (get-in record ["coverage" "flags"]))
        event {"refreshedAt" refreshed-at "methodVersion" (get record "methodVersion") "inputsHash" h}
        hist (vec (get record "refreshHistory" []))
        dup? (boolean (some #(and (= (get % "refreshedAt") refreshed-at)
                                  (= (get % "inputsHash") h)) hist))]
    (if dup?
      [record false]
      [(assoc record "refreshHistory" (conj hist event)) true])))

;; ── Hyakka proposal / readback ────────────────────────────────────────────────

(defn hyakka-proposal
  "AU — build an audit-only Hyakka proposal payload from a validated record.
  Requests NO action: no contact, no funding, no sponsorship, no hiring, no venue,
  no publication decision. The proposal carries the full provenance chain and the
  interpretation constraints verbatim."
  [record]
  (when-not (= (get-in record ["interpretation" "noRanking"]) true)
    (refuse "AU/PROPOSAL" "a proposal is built only from a record carrying the interpretation constraints"))
  {"kind" "research-influence-observation-proposal"
   "requestedAction" "audit-only"
   "noAction" true
   "methodVersion" (get record "methodVersion")
   "observationId" (get record "observationId")
   "record" record
   "outOfScope" ["contact" "funding" "sponsorship" "hiring" "venue-decision" "publication-decision" "policy-decision"]
   "outOfScopeReasons"
   {"citationNotSupport" "citation is not support"
    "fundingNotEndorsement" "funding is not endorsement"
    "attentionNotImpact" "attention is not impact"
    "correlationNotCausation" "correlation is not causation"
    "noRanking" "never rank researchers"}})

(defn hyakka-readback
  "Read back what Hyakka stored and verify it against the proposal that was sent.
  Pure: returns {matches boolean diffs [..]}. Verifies (1) the record bytes-equal,
  (2) every interpretation boolean is present and true in the stored copy,
  (3) the refresh history is a superset-by-prefix of what was sent (append-only held)."
  [proposal stored]
  (let [sent (get proposal "record" {})
        diffs (vec (concat
                     (when-not (= sent (get stored "record"))
                       ["record-mismatch"])
                     (for [[k v] INTERPRETATION
                           :when (not= v (get-in stored ["record" "interpretation" k]))]
                       (str "interpretation/" k "-violated"))
                     (let [hs (get-in stored ["record" "refreshHistory"])]
                       (when-not (every? (set hs) (get sent "refreshHistory"))
                         ["refresh-history-shrunk"]))))]
    {"matches" (empty? diffs) "diffs" diffs}))
