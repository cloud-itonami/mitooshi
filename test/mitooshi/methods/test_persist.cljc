(ns mitooshi.methods.test-persist
  "Cross-language oracle tests for mitooshi.methods.persist — the Clojure port of
  methods/persist.py (the bridge-free core).

  test_persist.py uses bridge() purely as a fixture generator; bridge is a
  separate (kakaku-coupled, unported) module, so the EXACT bridge output for
  observed-at 1 and 2 was captured by running the REAL Python bridge and embedded
  verbatim here (7 series / 7 obs per snapshot). The persist behaviour assertions
  are then identical to the Python: append-only / idempotent / additive / never
  mutated, union-first-wins series, EDN round-trips through the same reader, two
  snapshots accumulate on disk, header marks DERIVED + G10-gated (非終末論)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [mitooshi.methods.persist :as persist]
            [mitooshi.methods.analyze :as analyze]))

;; ── captured REAL bridge output (the cross-language oracle fixtures) ──────────
(def b1-series
  {"s-malacca-transit"     {":series/id" "s-malacca-transit" ":series/kind" ":transit-load" ":series/unit" "vessels"}
   "s-suez-red-sea-transit" {":series/id" "s-suez-red-sea-transit" ":series/kind" ":transit-load" ":series/unit" "vessels"}
   "s-hormuz-transit"      {":series/id" "s-hormuz-transit" ":series/kind" ":transit-load" ":series/unit" "vessels"}
   "s-luzon-strait-transit" {":series/id" "s-luzon-strait-transit" ":series/kind" ":transit-load" ":series/unit" "vessels"}
   "s-malacca-cable"       {":series/id" "s-malacca-cable" ":series/kind" ":cable-load" ":series/unit" "Tbps"}
   "s-luzon-strait-cable"  {":series/id" "s-luzon-strait-cable" ":series/kind" ":cable-load" ":series/unit" "Tbps"}
   "s-suez-red-sea-cable"  {":series/id" "s-suez-red-sea-cable" ":series/kind" ":cable-load" ":series/unit" "Tbps"}})

(defn- obs-at [ts]
  (mapv (fn [[sid v actor]]
          {":obs/id" (str "obs." sid "." ts) ":obs/series" sid
           ":obs/observed-at" ts ":obs/value" v ":obs/source-actor" actor})
        [["s-malacca-transit" 3.0 "watari"]
         ["s-suez-red-sea-transit" 1.0 "watari"]
         ["s-hormuz-transit" 1.0 "watari"]
         ["s-luzon-strait-transit" 2.0 "watari"]
         ["s-malacca-cable" 940.16 "watatsuna"]
         ["s-luzon-strait-cable" 680.56 "watatsuna"]
         ["s-suez-red-sea-cable" 349.6 "watatsuna"]]))

(def b1-obs (obs-at 1))
(def b2-obs (obs-at 2))

(deftest append-to-empty-trail-adds-all
  (let [[merged added dup] (persist/append-obs [] b1-obs)]
    (is (= (count b1-obs) added))
    (is (= 0 dup))
    (is (= (count b1-obs) (count merged)))))

(deftest reappend-same-snapshot-is-idempotent
  (let [[merged _ _] (persist/append-obs [] b1-obs)
        [merged2 added2 dup2] (persist/append-obs merged b1-obs)]
    (is (= 0 added2))
    (is (= (count b1-obs) dup2))                      ; nothing new, all duplicates
    (is (= (count merged) (count merged2)))))         ; trail unchanged

(deftest new-snapshot-is-additive-never-removes
  (let [[merged _ _] (persist/append-obs [] b1-obs)
        before-ids (into #{} (map #(get % ":obs/id") merged))
        [merged2 added2 dup2] (persist/append-obs merged b2-obs)
        after-ids (into #{} (map #(get % ":obs/id") merged2))]
    (is (every? after-ids before-ids))                ; 非終末論: never drops an obs
    (is (= (count b2-obs) added2))
    (is (= 0 dup2))))                                  ; different ts → all new

(deftest existing-obs-values-are-not-mutated
  (let [[merged _ _] (persist/append-obs [] b1-obs)
        snapshot (into {} (map (fn [o] [(get o ":obs/id") (get o ":obs/value")]) merged))
        [merged2 added _] (persist/append-obs merged b1-obs)
        after (into {} (map (fn [o] [(get o ":obs/id") (get o ":obs/value")]) merged2))]
    (is (= snapshot after))
    (is (= 0 added))))

(deftest merge-series-is-union-first-wins
  (let [merged (persist/merge-series {} b1-series)]
    (is (= (set (keys b1-series)) (set (keys merged))))
    (is (= merged (persist/merge-series merged b1-series)))))   ; re-merge stable

(deftest emit-round-trips-through-reader
  (let [[merged-obs _ _] (persist/append-obs [] b1-obs)
        edn (persist/emit-trail-edn b1-series merged-obs)
        recs (analyze/read-edn edn)
        obs (filter #(contains? % ":obs/id") recs)
        series (filter #(contains? % ":series/id") recs)]
    (is (= (count merged-obs) (count obs)))
    (is (= (count b1-series) (count series)))
    (is (= (sort (map #(get % ":obs/value") merged-obs))
           (sort (map #(get % ":obs/value") obs))))))  ; values survive the round-trip

(deftest emit-header-marks-derived-and-gated
  (let [[merged-obs _ _] (persist/append-obs [] b1-obs)
        edn (persist/emit-trail-edn b1-series merged-obs)]
    (is (str/includes? edn "APPEND-ONLY"))
    (is (str/includes? edn "DERIVED"))
    (is (str/includes? edn "G10-gated"))
    (is (str/includes? edn "非終末論"))))

#?(:clj
   (deftest persist-to-disk-two-snapshots-accumulate
     (let [tmp (java.io.File/createTempFile "mitooshi-trail" ".edn")
           path (.getAbsolutePath tmp)]
       (.delete tmp)                                   ; first persist sees a missing file
       (try
         (let [s1 (persist/persist path {"series" b1-series "obs" b1-obs})
               s2 (persist/persist path {"series" b1-series "obs" b2-obs})
               s3 (persist/persist path {"series" b1-series "obs" b2-obs})]  ; idempotent re-run
           (is (> (get s1 "added") 0))
           (is (= (get s1 "added") (get s2 "added")))
           (is (= 0 (get s2 "duplicate")))
           (is (= 0 (get s3 "added")))
           (is (= (get s2 "added") (get s3 "duplicate")))
           (let [[_ obs] (persist/load-trail path)
                 ats (sort (distinct (map #(get % ":obs/observed-at") obs)))]
             (is (= [1 2] (vec ats)))))
         (finally (.delete (java.io.File. path)))))))
