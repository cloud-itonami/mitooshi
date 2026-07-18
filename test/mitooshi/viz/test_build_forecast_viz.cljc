(ns mitooshi.viz.test-build-forecast-viz
  "Tests for the mitooshi 見通し forecast-viz payload builder (viz/build_forecast_viz.cljc).
  1:1 port of test/mitooshi/viz/test_build_forecast_viz.cljc: the payload carries leak-free history + a distribution
  forecast (G1 not a point / G2 :resilience / G5 info-as-of < target), and render-html inlines a
  self-contained payload (no live http fetch)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [mitooshi.viz.build-forecast-viz :as b]))

(def here (io/file "docs/viz"))

(deftest test-payload-has-history-and-forecast
  (let [p (b/build-payload 7)]
    (is (= 7 (count (get p "history"))))
    (is (some? (get p "forecast")))))

(deftest test-forecast-is-distribution-not-point-g1
  (let [fc (get (b/build-payload) "forecast")]
    (is (= false (get fc "pointAsserted")))           ; G1
    (is (= "gaussian" (get fc "distKind")))
    (is (< (get-in fc ["band95" 0]) (get fc "mean") (get-in fc ["band95" 1])))
    (is (and (>= (get-in fc ["band68" 0]) (get-in fc ["band95" 0]))
             (<= (get-in fc ["band68" 1]) (get-in fc ["band95" 1]))))))

(deftest test-forecast-use-is-resilience-g2
  (is (= ":resilience" (get (get (b/build-payload) "forecast") "use"))))   ; G2

(deftest test-forecast-is-leak-free-info-before-target
  (is (< (get (get (b/build-payload 7) "forecast") "infoAsOf") 7)))         ; G5

(deftest test-climatology-mean-matches-history
  ;; history t=1..6 = -0.45,-0.30,-0.15,0.0,0.15,0.30 → mean -0.075 (climatology, leak-free)
  (is (= -0.075 (get (get (b/build-payload 7) "forecast") "mean"))))

(deftest test-html-inlines-payload-self-contained
  (let [tpl (io/file here "_template.htm")]
    (when (.exists tpl)
      (let [html (b/render-html (b/build-payload) tpl)]
        (is (not (str/includes? html "/*__PAYLOAD__*/null")))
        (is (str/includes? html "s-jan-4901777300443-supply-demand"))
        (let [js (second (str/split html #"<script>"))
              leftover (str/replace js "http://www.w3.org/2000/svg" "")]
          (is (and (not (str/includes? leftover "http://")) (not (str/includes? leftover "https://")))))))))
