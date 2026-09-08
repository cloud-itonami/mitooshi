(ns mitooshi.methods.research-observation
  "mitooshi 見通し — research-influence-observation/v1 (R0, offline, pure).

  A bounded contract for recording RESEARCH INFLUENCE observations (the
  :impact-observation entity of research-scope.edn) with provenance, an explicit
  measurement window, and honesty flags — WITHOUT researcher ranking and WITHOUT
  causal claims. Reuses mitooshi's G4 membrane pattern (source-class admission)
  and G11 honesty pattern (append-only, sorted, non-terminal observations).

  CONSTITUTIONAL (research-scope.edn):
    - source-policy :impact-allow — a signal is admitted ONLY from
      {:citation-registry :official-policy-document :official-patent-record
       :standards-body-first-party :guideline-publisher-first-party
       :publisher-correction-or-retraction};
      search snippets / generated summaries / scraped profiles are REFUSED.
    - required-common-fields — source-url, source-class, source-language,
      observed-at, content-hash, issuing-organization, original-title must all
      be present; missing provenance is refused, never defaulted.
    - epistemic boundaries — citation is not positive support; funding is not
      endorsement; correlation is not causation; a metric is a versioned
      observation, not timeless impact; missing is unmeasured.
    - additive tallies only: polarities are NEVER netted, retractions and
      corrections are preserved as their own dimension, nothing is ranked.
    - coverage is :partial — a window with nothing measured yields NO proposal
      (absence of evidence is not fabricated evidence).

  Pure: no I/O, no network, no LLM. stdlib only."
  (:require [kotoba.lang.text :as str]))

(def CONTRACT {:contract/id "research-influence-observation"
               :contract/version "v1"
               :method/version "research-influence-observation/v1"})

;; research-scope.edn :source-policy :impact-allow — the ONLY admitted classes.
(def ALLOWED-IMPACT-SOURCE-CLASS
  #{:citation-registry :official-policy-document :official-patent-record
    :standards-body-first-party :guideline-publisher-first-party
    :publisher-correction-or-retraction})

;; research-scope.edn :source-policy :forbid — refused outright, counted + reasoned.
(def FORBIDDEN-SOURCE-CLASS
  #{:search-snippet :generated-summary :third-party-wiki-prose :scraped-profile
    :paywall-bypass :captcha-bypass :inferred-sponsorship :inferred-causality})

(def IMPACT-DIMENSIONS
  #{:scholarly-citation :replication :correction :retraction
    :policy-citation :patent-citation :standard-adoption
    :clinical-guideline-citation :dataset-or-software-reuse})

(def REQUIRED-SOURCE-FIELDS
  [:source-url :source-class :source-language :observed-at
   :content-hash :issuing-organization :original-title])

(defn- kw [v] (if (keyword? v) v (keyword (str/replace (str (or v "")) #"^:+" ""))))

(defn- missing-fields
  [source]
  (vec (remove #(some? (get source %)) REQUIRED-SOURCE-FIELDS)))

(defn- admit
  "Returns [verdict signal-reason]. :admit | :refuse (forbidden class / missing
  provenance / unknown dimension). Refusals are counted and reasoned — never
  silently relabelled (G4 pattern)."
  [{:keys [dimension source] :as _signal}]
  (let [d (kw dimension)
        sc (kw (get source :source-class))
        missing (missing-fields source)]
    (cond
      (contains? FORBIDDEN-SOURCE-CLASS sc)
      [:refuse {:reason :forbidden-source-class :source-class sc}]
      (empty? missing)
      (if (contains? ALLOWED-IMPACT-SOURCE-CLASS sc)
        (if (contains? IMPACT-DIMENSIONS d)
          [:admit nil]
          [:refuse {:reason :unknown-dimension :dimension d}])
        [:refuse {:reason :source-class-not-in-impact-allow :source-class sc}])
      :else
      [:refuse {:reason :missing-required-source-fields :missing missing}])))

(defn- in-window?
  [window observed-at]
  (let [{:keys [from to]} window
        o observed-at]
    (and (some? o)
         (or (nil? from) (pos? (compare o from)))
         (or (nil? to) (neg? (compare o to))))))

(defn- tallies
  "Additive per-dimension counts. Corrections/retractions are tallied as their
  own dimensions and are NEVER netted against scholarly-citation counts."
  [admitted]
  (->> admitted
       (map #(kw (:dimension %)))
       (frequencies)
       (into (sorted-map))))

(defn observe
  "signals: [{:id :dimension :observed-at :source {required fields...}}]
   window:  {:from \"2024-01-01\" :to \"2025-01-01\"} (either bound may be nil).
  Returns an immutable observation map stamped with :method/version. Pure:
  same input → byte-identical output."
  [signals window]
  (let [{admitted true refused false}
        (group-by (fn [s] (= :admit (first (admit s)))) signals)
        refused+ (mapv (fn [s]
                         (let [[_ r] (admit s)]
                           (merge {:id (:id s)} r)))
                       refused)
        excluded (vec (sort (map :id (remove #(in-window? window (:observed-at %)) admitted))))
        in-window (filterv #(in-window? window (:observed-at %)) admitted)
        sorted-in (sort-by (juxt :observed-at :id) in-window)
        counts (tallies sorted-in)
        retraction-present? (boolean (or (get counts :retraction)
                                         (get counts :correction)))
        source-classes (distinct (map #(kw (get-in % [:source :source-class]))
                                      sorted-in))
        observation
        {:observation/contract (:contract/id CONTRACT)
         :observation/version (:contract/version CONTRACT)
         :method/version (:method/version CONTRACT)
         :window {:from (:from window) :to (:to window)}
         :signals/sorted (mapv #(select-keys % [:id :dimension :observed-at :source])
                               sorted-in)
         :tallies/additive counts
         :excluded/out-of-window excluded
         :refused (into (sorted-map-by compare)
                        (frequencies (map :reason refused+)))
         :refused/detail (vec (sort-by :id refused+))
         :flags {:missing-is-unmeasured true
                 :excluded-signals-present (boolean (seq excluded))
                 :retraction-present retraction-present?
                 :single-source-dependency (= 1 (count source-classes))}
         :coverage :partial
         :ranking nil
         :ranking-forbidden true
         :causal-claims-forbidden true
         :claims []}]
    observation))

(defn refresh-history
  "Append a new observation to an existing refresh history. Prior observations
  are immutable; history is append-only and sorted by window.from."
  [history observation]
  (sort-by (comp :from :window) (conj (vec history) observation)))

(defn measured?
  [observation]
  (pos? (reduce + 0 (vals (get observation :tallies/additive)))))

(defn- fnv-1a-hex
  "Deterministic FNV-1a over the string, in both CLJ and CLJS: the multiply is
  done on integers below 2^53 (JS doubles stay exact), then reduced mod 2^32.
  Same input string → same hex digest on every runtime."
  [s]
  (let [hex "0123456789abcdef"
        n (count s)]
    (loop [i 0 h 0x811c9dc5]
      (if (= i n)
        (apply str
               (map #(nth hex %)
                    [(bit-and (bit-shift-right h 28) 15)
                     (bit-and (bit-shift-right h 24) 15)
                     (bit-and (bit-shift-right h 20) 15)
                     (bit-and (bit-shift-right h 16) 15)
                     (bit-and (bit-shift-right h 12) 15)
                     (bit-and (bit-shift-right h 8) 15)
                     (bit-and (bit-shift-right h 4) 15)
                     (bit-and h 15)]))
        (let [c (int (nth s i))
              h2 (mod (* (bit-xor h c) 0x01000193) 0x100000000)]
          (recur (inc i) h2))))))

(defn dedupe-key
  "Pure, deterministic identity over contract + method/version + window +
  sorted tallies + sorted refusal reasons + out-of-window exclusions. Two runs
  over the same measurement produce the same key, so Hyakka can refuse
  duplicate proposals instead of accumulating copies; a changed tally, window,
  refusal set or method-version changes the key (a changed measurement is a
  new observation). Everything entering the key is serialized through
  sorted-map / sorted vectors, so map iteration order cannot leak into it.
  Degenerate input (nothing measured) yields nil — no fabricated identity."
  [observation]
  (when (and (measured? observation)
             (not (nil? (get observation :window))))
    (let [payload
          {:contract/id (:observation/contract observation)
           :contract/version (:observation/version observation)
           :method/version (:method/version observation)
           :window (into (sorted-map) (get observation :window))
           :tallies (into (sorted-map) (get observation :tallies/additive))
           :refused (into (sorted-map) (get observation :refused))
           :excluded (vec (sort (get observation :excluded/out-of-window)))}]
      (str "research-influence-observation/"
           (:contract/version CONTRACT) "/"
           (fnv-1a-hex (pr-str payload))))))

(defn hyakka-proposal
  "A Hyakka (wiki) proposal is emitted ONLY when something was actually
  measured inside the window. Nothing measured → nil (no fabrication). The
  proposal suggests auditable observation questions — never rankings, never
  causal claims, never funding/sponsorship/hiring/venue/policy decisions."
  [observation]
  (when (measured? observation)
    {:proposal/target "network-awai/app-hyakka"
     :proposal/contract (:contract/id CONTRACT)
     :proposal/version (:contract/version CONTRACT)
     :proposal/method-version (:method/version CONTRACT)
     :proposal/window (:window observation)
     :proposal/dedupe-key (dedupe-key observation)
     :proposal/tallies (get observation :tallies/additive)
     :proposal/flags (get observation :flags)
     :ranking nil
     :ranking-forbidden true
     :causal-claims-forbidden true
     :claims []
     :proposal/provenance (mapv (fn [s]
                                  (select-keys (:source s)
                                               [:source-url :source-class
                                                :source-language :observed-at
                                                :content-hash :issuing-organization
                                                :original-title]))
                                (get observation :signals/sorted))
     :proposal/questions
     (vec (concat
           (when (get-in observation [:flags :retraction-present])
             ["Which corrections/retractions are recorded alongside the cited works (correction is not a deletion of the observation)?"])
           (when (get-in observation [:flags :single-source-dependency])
             ["Which single source class dominates this window, and what independent registry would falsify it?"])
           ["What is the coverage gap of this partial window (which disciplines/jurisdictions/languages are unmeasured)?"]))}))

(defn readback
  "Reject tampering: a readback that has lost provenance, swapped its contract
  identity, or had its safety guards removed is refused (counted + reasoned)."
  [proposal]
  (cond
    (not= (:proposal/contract proposal) (:contract/id CONTRACT))
    {:accepted false :reason :contract-identity-mismatch}
    (not= (:proposal/version proposal) (:contract/version CONTRACT))
    {:accepted false :reason :contract-version-mismatch}
    (or (empty? (:proposal/provenance proposal))
        (some #(not-every? (fn [f] (contains? % f)) REQUIRED-SOURCE-FIELDS)
              (:proposal/provenance proposal))
        (some #(some nil? (vals (select-keys % REQUIRED-SOURCE-FIELDS)))
              (:proposal/provenance proposal)))
    {:accepted false :reason :provenance-lost}
    (or (not (true? (:ranking-forbidden proposal)))
        (not (true? (:causal-claims-forbidden proposal)))
        (some? (:ranking proposal))
        (seq (:claims proposal)))
    {:accepted false :reason :safety-guards-removed}
    :else
    {:accepted true :proposal proposal}))
