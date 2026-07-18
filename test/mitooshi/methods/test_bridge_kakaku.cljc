(ns mitooshi.methods.test-bridge-kakaku
  "Cross-language oracle tests for mitooshi.methods.bridge-kakaku — the Clojure port of
  methods/bridge_kakaku.py (the pure price/supply-demand bridge core).

  Ported 1:1 from the REAL Python test_bridge_kakaku.py: runs over the committed
  kakaku-sample.edn through the already-ported analyze/load-edn-file, so the per-product
  price-level + supply-demand-index series, the non-price/SD skip count, and the carried
  values/source actors are pinned identically to the Python."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.bridge-kakaku :as bk]
            [mitooshi.methods.analyze :as analyze]))

(def sample-path "data/bridge/kakaku-sample.edn")
(defn- records [] (analyze/load-edn-file sample-path))

(deftest maps-price-and-supply-demand-series
  (let [ids (set (keys (get (bk/bridge-kakaku (records) 1) "series")))]
    (is (contains? ids "s-jan-4901777300443-price"))
    (is (contains? ids "s-jan-4901777300443-supply-demand"))
    (is (contains? ids "s-gtin-04901234567894-price"))
    (is (contains? ids "s-gtin-04901234567894-supply-demand"))))

(deftest ignores-non-price-sd-records
  (let [b (bk/bridge-kakaku (records) 1)]
    (is (= 3 (get b "skipped")))                      ; 1 spread + 1 offer + 1 merchant
    (is (= 4 (count (get b "series"))))))             ; 2 products × {price, supply-demand}

(deftest carries-value-and-source-actor
  (let [b (bk/bridge-kakaku (records) 5)
        price (first (filter #(= "s-jan-4901777300443-price" (get % ":obs/series")) (get b "obs")))
        sd (first (filter #(= "s-jan-4901777300443-supply-demand" (get % ":obs/series")) (get b "obs")))]
    (is (= 3200.0 (get price ":obs/value")))
    (is (= "kakaku" (get price ":obs/source-actor")))
    (is (= 5 (get price ":obs/observed-at")))
    (is (= 0.42 (get sd ":obs/value")))))

(deftest source-class-is-public-broadcast-g4
  (let [s (get-in (bk/bridge-kakaku (records) 1) ["series" "s-jan-4901777300443-supply-demand"])]
    (is (= ":public-broadcast" (get s ":series/source-class")))     ; G4
    (is (= ":representative" (get s ":series/sourcing")))           ; G11
    (is (= ":supply-demand-index" (get s ":series/kind")))))

(deftest obs-chain-into-a-forecast-trail
  (let [b1 (bk/bridge-kakaku (records) 1)
        b2 (bk/bridge-kakaku (records) 2)
        trail (filter #(= "s-jan-4901777300443-supply-demand" (get % ":obs/series"))
                      (concat (get b1 "obs") (get b2 "obs")))
        ats (sort (map #(get % ":obs/observed-at") trail))]
    (is (= [1 2] (vec ats)))))                        ; append-only as-of trail (非終末論)
