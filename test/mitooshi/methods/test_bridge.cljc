(ns mitooshi.methods.test-bridge
  "Cross-language oracle tests for mitooshi.methods.bridge — the Clojure port of
  methods/bridge.py (the pure `bridge` core).

  Ported 1:1 from the REAL Python test_bridge.py: runs over the committed
  watari-sample.edn / watatsuna-sample.edn through the already-ported
  analyze/load-edn-file, so the chokepoint join (watari transit ∩ watatsuna cable on
  the shared keyword), the non-chokepoint skip count, and the carried values/source
  actors are pinned identically to the Python."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.bridge :as bridge]
            [mitooshi.methods.analyze :as analyze]))

(def bridge-dir "data/bridge")

(defn- by-actor []
  {"watari" (analyze/load-edn-file (str bridge-dir "/watari-sample.edn"))
   "watatsuna" (analyze/load-edn-file (str bridge-dir "/watatsuna-sample.edn"))})

(deftest bridge-maps-chokepoints-to-series
  (let [b (bridge/bridge (by-actor) 1)
        ids (set (keys (get b "series")))]
    (is (contains? ids "s-malacca-transit"))
    (is (contains? ids "s-malacca-cable"))
    (is (contains? ids "s-luzon-strait-transit"))
    (is (contains? ids "s-luzon-strait-cable"))))

(deftest bridge-ignores-non-chokepoint-records
  (let [b (bridge/bridge (by-actor) 1)]
    (is (= 3 (get b "skipped")))                      ; 1 lane + 1 craft + 1 station
    (is (= 7 (count (get b "series"))))))             ; 4 watari + 3 watatsuna

(deftest bridge-carries-value-and-source-actor
  (let [b (bridge/bridge (by-actor) 5)
        malacca-t (first (filter #(= "s-malacca-transit" (get % ":obs/series")) (get b "obs")))
        malacca-c (first (filter #(= "s-malacca-cable" (get % ":obs/series")) (get b "obs")))]
    (is (= 3.0 (get malacca-t ":obs/value")))
    (is (= "watari" (get malacca-t ":obs/source-actor")))
    (is (= 5 (get malacca-t ":obs/observed-at")))
    (is (= 940.16 (get malacca-c ":obs/value")))
    (is (= "watatsuna" (get malacca-c ":obs/source-actor")))))

(deftest bridge-same-chokepoint-two-series-two-units
  (let [b (bridge/bridge (by-actor) 1)]
    (is (= ":transit-load" (get-in b ["series" "s-malacca-transit" ":series/kind"])))
    (is (= ":cable-load" (get-in b ["series" "s-malacca-cable" ":series/kind"])))
    (is (= ":public-broadcast" (get-in b ["series" "s-malacca-cable" ":series/source-class"])))))  ; G4

(deftest bridge-single-actor-ok
  (let [b (bridge/bridge {"watari" (analyze/load-edn-file (str bridge-dir "/watari-sample.edn"))} 1)]
    (is (= 4 (count (get b "series"))))
    (is (every? #(contains? % ":obs/source-actor") (get b "obs")))))

(deftest bridged-obs-chain-into-a-forecast-series
  (let [b1 (bridge/bridge (by-actor) 1)
        b2 (bridge/bridge (by-actor) 2)
        trail (filter #(= "s-malacca-cable" (get % ":obs/series"))
                      (concat (get b1 "obs") (get b2 "obs")))
        ats (sort (map #(get % ":obs/observed-at") trail))]
    (is (= [1 2] (vec ats)))))                        ; append-only as-of trail (非終末論)
