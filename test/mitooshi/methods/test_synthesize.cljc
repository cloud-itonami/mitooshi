(ns mitooshi.methods.test-synthesize
  "Cross-language oracle tests for mitooshi.methods.synthesize — the Clojure port
  of methods/synthesize.py.

  Ported 1:1 from the REAL Python test_synthesize.py (the cross-language oracle):
  every composite assertion runs over the committed Datom artifacts
  (chokepoint-trail + chokepoint-forecast .kotoba.edn) through the already-ported
  analyze/load-edn-file, so any divergence in the watari+watatsuna+mitooshi fusion
  or the scale-free attention blend fails here."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [mitooshi.methods.synthesize :as syn]
            [mitooshi.methods.analyze :as analyze]))

(def trail-path "data/persisted/chokepoint-trail.kotoba.edn")
(def forecast-path "data/persisted/chokepoint-forecast.kotoba.edn")

(defn- comp* []
  (syn/synthesize (analyze/load-edn-file trail-path)
                  (analyze/load-edn-file forecast-path)))

(deftest chokepoint-extraction-strips-suffixes
  (is (= ":malacca" (syn/chokepoint-of "s-malacca-cable")))
  (is (= ":luzon-strait" (syn/chokepoint-of "s-luzon-strait-transit"))))

(deftest latest-by-series-takes-max-observed-at
  (let [rows [{":obs/series" "s-x" ":obs/observed-at" 1 ":obs/value" 10.0}
              {":obs/series" "s-x" ":obs/observed-at" 3 ":obs/value" 30.0}
              {":obs/series" "s-x" ":obs/observed-at" 2 ":obs/value" 20.0}]]
    (is (= 30.0 (get (syn/latest-by-series rows) "s-x")))))

(deftest composite-ranks-malacca-top
  (testing "highest transit AND cable → top attention"
    (is (= ":malacca" (get (first (comp*)) "chokepoint")))))

(deftest attention-is-bounded-and-sorted-desc
  (let [atts (mapv #(get % "attention") (comp*))]
    (is (every? #(<= 0.0 % 1.0) atts))
    (is (= atts (vec (sort > atts))))))

(deftest attention-formula-blend
  (let [comp (comp*)
        cables (keep #(get % "cable_load") comp)
        transits (keep #(get % "transit") comp)
        mc (apply max cables)
        mt (apply max transits)]
    (doseq [d comp]
      (let [cl (get d "cable_load") tr (get d "transit")
            nc (if cl (/ cl mc) 0.0)
            nt (if tr (/ tr mt) 0.0)
            expected (/ (Math/rint (* (+ (* 0.7 nc) (* 0.3 nt)) 10000.0)) 10000.0)]
        (is (< (Math/abs (- (get d "attention") expected)) 1e-9))))))

(deftest composite-joins-forecast
  (testing "at least one chokepoint carries a forecast cable mean (joined from the forecast artifact)"
    (is (some #(some? (get % "forecast_cable_mean")) (comp*)))))

(deftest transit-only-chokepoint-has-low-attention
  (let [hormuz (first (filter #(= ":hormuz" (get % "chokepoint")) (comp*)))]
    (when hormuz                                      ; hormuz has transit but no cable in seed
      (is (nil? (get hormuz "cable_load")))
      (is (< (get hormuz "attention") 0.5)))))

(deftest render-edn-is-resilience-not-target-list
  (let [edn (syn/render-edn (comp*))]
    (is (str/includes? edn "RESILIENCE"))
    (is (str/includes? (str/lower-case edn) "never a target-list"))
    (is (str/includes? edn ":choke/attention"))
    (is (str/includes? edn "G10-gated"))))
