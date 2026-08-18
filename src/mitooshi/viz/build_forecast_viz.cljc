(ns mitooshi.viz.build-forecast-viz
  "mitooshi 見通し — forecast fan-chart viz payload builder. 1:1 port of src/mitooshi/viz/build_forecast_viz.cljc
  (the pure build-payload + render-html; the __main__ file-writing CLI is the omitted I/O leg).
  Bridges a rising kakaku supply-demand series, forecasts the next value as a Gaussian distribution,
  and shapes both into a self-contained fan-chart payload (G1 distribution / G2 resilience / G5 leak-free)."
  (:require [clojure.string :as str]
            [mitooshi.methods.bridge-kakaku :as bk]
            [mitooshi.methods.forecast :as fc]
            [json.compat :as json]))

(def pid "jan_4901777300443")
(def sid "s-jan-4901777300443-supply-demand")
(defn- round4 [x] (/ (Math/round (* (double x) 10000.0)) 10000.0))

(defn build-payload
  ([] (build-payload 7))
  ([target-at]
   (let [acc (reduce (fn [a t]
                       (let [idx (round4 (+ -0.6 (* 0.15 t)))
                             b (bk/bridge-kakaku [{":sd/product" pid ":sd/index" idx}] t)]
                         (-> a (update :series merge (get b "series")) (update :obs into (get b "obs")))))
                     {:series {} :obs []} (range 1 (inc target-at)))
         rows (into (vec (vals (:series acc))) (:obs acc))
         hist (get (fc/series-histories rows) sid)
         history (mapv (fn [[t v]] {"t" t "v" (round4 v)}) hist)
         f (fc/forecast-next sid hist target-at)
         forecast (when f
                    (let [mean (:mean f) sd (:sd f)]
                      {"target" target-at "infoAsOf" (:info-as-of f) "mean" mean "sd" sd
                       "band68" [(round4 (- mean sd)) (round4 (+ mean sd))]
                       "band95" [(round4 (- mean (* 2 sd))) (round4 (+ mean (* 2 sd)))]
                       "distKind" (:dist-kind f) "use" (:use f) "pointAsserted" (:point-asserted f)}))]
     {"generator" "mitooshi/viz/build_forecast_viz.cljc"
      "series" sid "unit" "supply-demand-index" "history" history "forecast" forecast
      "intent" "distribution-forecast → resilience (never a point, never a trade)"})))

(defn render-html
  "Inline the payload JSON into the self-contained template (mirror of render_html)."
  [payload template]
  (str/replace (slurp (str template)) "/*__PAYLOAD__*/null" #?(:clj (json/generate-string payload) :cljs (str payload))))
