(ns mitooshi.methods.test-kakaku-forecast-e2e
  "End-to-end: kakaku 価格 → bridge-kakaku → mitooshi forecast (cross-actor pipeline). 1:1 port of
  methods/test_kakaku_forecast_e2e.py. kakaku supply-demand-index obs bridged into mitooshi series+obs
  are forecast as a DISTRIBUTION to :resilience (G1/G2), leak-free (G5), scored with skill (G12)."
  (:require [clojure.test :refer [deftest is]]
            [mitooshi.methods.bridge-kakaku :as bk]
            [mitooshi.methods.forecast :as fc]
            [mitooshi.methods.score :as score]))

(def pid "jan_4901777300443")
(def sid "s-jan-4901777300443-supply-demand")

(defn- round4 [x] (/ (Math/round (* (double x) 10000.0)) 10000.0))

(defn- bridged-trail []
  (let [acc (reduce (fn [a t]
                      (let [idx (round4 (+ -0.6 (* 0.15 t)))
                            b (bk/bridge-kakaku [{":sd/product" pid ":sd/index" idx}] t)]
                        (-> a (update :series merge (get b "series")) (update :obs into (get b "obs")))))
                    {:series {} :obs []} (range 1 8))]
    (into (vec (vals (:series acc))) (:obs acc))))

(deftest test-pipeline-builds-a-forecastable-trail
  (let [hist (fc/series-histories (bridged-trail))]
    (is (contains? hist sid))
    (let [ts (mapv first (get hist sid))]
      (is (= ts (vec (sort ts)))) (is (= ts [1 2 3 4 5 6 7])))))

(deftest test-kakaku-series-forecast-is-distribution-to-resilience
  (let [f (fc/forecast-next sid (get (fc/series-histories (bridged-trail)) sid) 7)]
    (is (some? f))
    (is (= false (:point-asserted f)))
    (is (= ":resilience" (:use f)))
    (is (= "gaussian" (:dist-kind f)))))

(deftest test-kakaku-forecast-is-leak-free
  (let [f (fc/forecast-next sid (get (fc/series-histories (bridged-trail)) sid) 7)]
    (is (< (:info-as-of f) 7))
    (is (contains? (score/score-pair f (score/->observation "o" :observed-at 7 :value 0.45)) "crps"))))

(deftest test-kakaku-forecast-trail-scores-against-realizing-obs
  (let [out (fc/forecast-trail (bridged-trail) 7 "climatology")
        row (first (filter #(= sid (get % "series")) out))]
    (is (and (contains? row "crps") (contains? row "skill")))
    (is (= ":resilience" (:use (get row "forecast"))))))
