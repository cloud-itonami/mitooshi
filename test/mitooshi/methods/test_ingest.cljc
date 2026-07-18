(ns mitooshi.methods.test-ingest
  "Cross-language oracle tests for mitooshi.methods.ingest — the Clojure port of
  methods/ingest.py (the pure `normalize` core).

  Ported from the REAL Python test_ingest.py: the 5 normalize-logic assertions run
  over the same committed wire/fixtures/sample-batch.json (2 public series +
  1 proprietary-terminal series refused), so any divergence in the G4 source
  membrane, the keyword normalization, or the append-only obs ordering fails here.

  The Python test's two --live cases (6 & 7) exercise main's CLI G10 gate via
  subprocess; main is omitted from this port per the convention (the G10 live-fetch
  gate stays in the Python entry point), so those CLI cases are out of scope here."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.ingest :as ingest]))

(def sample-path "wire/fixtures/sample-batch.json")

(defn- batch [] (ingest/load-batch sample-path))

(deftest sample-batch-normalizes-two-public-series
  (let [n (ingest/normalize (batch))]
    (is (= #{"s-hormuz-transit" "s-port-congestion"} (set (keys (get n "series")))))))

(deftest proprietary-terminal-series-is-refused
  (let [n (ingest/normalize (batch))
        r (first (get n "refused"))]
    (is (= 1 (count (get n "refused"))))
    (is (= "s-blocked-terminal" (get r "id")))
    (is (clojure.string/includes? (get r "reason") "G4"))))

(deftest observations-are-sorted-append-only
  (let [n (ingest/normalize (batch))
        hormuz (filterv #(= "s-hormuz-transit" (get % ":obs/series")) (get n "obs"))
        ats (mapv #(get % ":obs/observed-at") hormuz)]
    (is (= ats (vec (sort ats))))                     ; 非終末論: append-only, latest = current
    (is (= 3 (last ats)))
    (is (= 2.7 (get (last hormuz) ":obs/value")))))

(deftest categorical-class-preserved
  (let [n (ingest/normalize (batch))
        cong (filter #(= "s-port-congestion" (get % ":obs/series")) (get n "obs"))]
    (is (some #(= "up" (get % ":obs/class")) cong))))

(deftest source-class-normalized-to-keyword
  (let [n (ingest/normalize (batch))
        hormuz (get-in n ["series" "s-hormuz-transit"])]
    (is (= ":public-broadcast" (get hormuz ":series/source-class")))
    (is (= ":representative" (get hormuz ":series/sourcing")))))
