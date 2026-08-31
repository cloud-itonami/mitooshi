(ns mitooshi.methods.test-research-observation
  "research-influence-observation/v1 — deterministic fixtures.
  Covers: provenance admission/refusal, out-of-window exclusion, tally
  determinism (byte-identical pr-str), honesty flags (missingness, retraction
  preservation, single-source dependency), append-only refresh history, and a
  tamper-rejecting Hyakka readback. Health alone is asserted as impact nowhere."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.research-observation :as rio]))

(def good-source
  {:source-url "https://doi.org/10.1000/example"
   :source-class :citation-registry
   :source-language "en"
   :observed-at "2025-03-01"
   :content-hash "sha256:0eb38bf2a01db942270120f6ab48b204b394ee53d16b3a83350b949cf6acba55"
   :issuing-organization "Crossref"
   :original-title "Example Work"})

(defn- sig [id dimension observed-at & [source]]
  {:id id :dimension dimension :observed-at observed-at
   :source (if source source good-source)})

(def window {:from "2024-01-01" :to "2026-01-01"})

(deftest provenance-admission
  (testing "allowed impact source classes are admitted"
    (let [obs (rio/observe [(sig "s1" :scholarly-citation "2025-03-01")] window)]
      (is (= {:scholarly-citation 1} (:tallies/additive obs)))
      (is (empty? (:refused/detail obs)))))
  (testing "forbidden source classes are refused, counted and reasoned"
    (let [bad (update good-source :source-class (constantly :scraped-profile))
          obs (rio/observe [(sig "s1" :scholarly-citation "2025-03-01" bad)] window)]
      (is (empty? (:tallies/additive obs)))
      (is (= 1 (get (:refused obs) :forbidden-source-class)))))
  (testing "missing required provenance is refused, never defaulted"
    (let [bad (dissoc good-source :content-hash)
          obs (rio/observe [(sig "s1" :scholarly-citation "2025-03-01" bad)] window)]
      (is (empty? (:tallies/additive obs)))
      (is (= [:content-hash] (-> obs :refused/detail first :missing)))))
  (testing "unknown dimension is refused"
    (let [obs (rio/observe [(sig "s1" :researcher-ranking "2025-03-01")] window)]
      (is (empty? (:tallies/additive obs)))
      (is (= 1 (get (:refused obs) :unknown-dimension))))))

(deftest time-window-refresh
  (testing "out-of-window signals are excluded and enumerated"
    (let [obs (rio/observe [(sig "in1" :scholarly-citation "2025-03-01")
                            (sig "old" :scholarly-citation "2023-12-31")
                            (sig "future" :scholarly-citation "2026-02-01")]
                           window)]
      (is (= {:scholarly-citation 1} (:tallies/additive obs)))
      (is (= ["future" "old"] (:excluded/out-of-window obs)))
      (is (get-in obs [:flags :excluded-signals-present]))))
  (testing "window refresh is append-only; prior observations immutable"
    (let [o1 (rio/observe [(sig "a" :scholarly-citation "2025-01-01")]
                          {:from "2024-01-01" :to "2025-07-01"})
          o2 (rio/observe [(sig "b" :policy-citation "2025-09-01")]
                          {:from "2025-07-01" :to "2026-01-01"})
          h  (rio/refresh-history [] o1)
          h' (rio/refresh-history h o2)]
      (is (= [o1] h))
      (is (= [o1 o2] h'))
      (is (= 2 (count h'))))))

(deftest honesty-flags
  (testing "retraction/correction preserved as own dimension, never netted"
    (let [obs (rio/observe [(sig "c" :scholarly-citation "2025-03-01")
                            (sig "r" :retraction "2025-04-01")
                            (sig "k" :correction "2025-05-01")]
                           window)]
      (is (= {:correction 1 :retraction 1 :scholarly-citation 1}
             (:tallies/additive obs)))
      (is (get-in obs [:flags :retraction-present]))
      ;; no netting: the observation never computes citations-minus-retractions
      (is (not (contains? (set (keys (:tallies/additive obs))) :net-impact)))))
  (testing "single-source dependency is flagged when only one source class seen"
    (let [obs (rio/observe [(sig "a" :scholarly-citation "2025-03-01")] window)]
      (is (get-in obs [:flags :single-source-dependency]))))
  (testing "multiple source classes clear the single-source flag"
    (let [other (assoc good-source :source-class :official-policy-document
                       :source-url "https://example.gov/policy")
          obs (rio/observe [(sig "a" :scholarly-citation "2025-03-01")
                            (sig "b" :policy-citation "2025-04-01" other)]
                           window)]
      (is (not (get-in obs [:flags :single-source-dependency]))))
    ))

(deftest guards-and-boundaries
  (let [obs (rio/observe [(sig "a" :scholarly-citation "2025-03-01")] window)]
    (testing "safety guards are structural, not prose"
      (is (nil? (:ranking obs)))
      (is (true? (:ranking-forbidden obs)))
      (is (true? (:causal-claims-forbidden obs)))
      (is (= [] (:claims obs))))
    (testing "method/version is stamped; coverage is partial"
      (is (= "research-influence-observation/v1" (:method/version obs)))
      (is (= :partial (:coverage obs))))))

(deftest determinism
  (testing "same input → byte-identical pr-str output"
    (let [signals [(sig "a" :scholarly-citation "2025-03-01")
                   (sig "b" :policy-citation "2025-04-01")]
          a (pr-str (rio/observe signals window))
          b (pr-str (rio/observe signals window))]
      (is (= a b)))))

(deftest hyakka-proposal-and-readback
  (testing "nothing measured → no proposal (no fabrication from absence)"
    (is (nil? (rio/hyakka-proposal
               (rio/observe [(sig "old" :scholarly-citation "2020-01-01")] window)))))
  (testing "measured → proposal with provenance and auditable questions"
    (let [obs (rio/observe [(sig "a" :scholarly-citation "2025-03-01")
                            (sig "r" :retraction "2025-04-01")]
                           window)
          p (rio/hyakka-proposal obs)]
      (is (some? p))
      (is (= 2 (count (:proposal/provenance p))))
      (is (seq (:proposal/questions p)))
      (is (= {:accepted true :proposal p} (rio/readback p)))))
  (testing "readback rejects tampering"
    (let [obs (rio/observe [(sig "a" :scholarly-citation "2025-03-01")] window)
          p (rio/hyakka-proposal obs)]
      (is (= {:accepted false :reason :contract-identity-mismatch}
             (rio/readback (assoc p :proposal/contract "impact-score"))))
      (is (= {:accepted false :reason :provenance-lost}
             (rio/readback (assoc p :proposal/provenance []))))
      (is (= {:accepted false :reason :safety-guards-removed}
             (rio/readback (assoc p :ranking [1 2 3]))))
      (is (= {:accepted false :reason :provenance-lost}
             (rio/readback (update-in p [:proposal/provenance 0]
                                      dissoc :source-url)))))))
