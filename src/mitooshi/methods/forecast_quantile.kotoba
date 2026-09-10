(ns mitooshi.methods.forecast-quantile
  "mitooshi 見通し — quantile (pinball-scored) forecaster. 1:1 Clojure port of
  methods/forecast_quantile.py (ADR-2606051800, R1, offline).

  A second forecaster family alongside the Gaussian baselines: emit a forecast as a set of
  QUANTILES (e.g. 10/50/90) rather than a mean±sd, and score it with the pinball (quantile)
  loss in score.cljc. Same constitutional invariants:

    G1 distribution-only — dist-kind=\"quantile\", point-asserted=false; a spread of quantiles
                           is a distribution, never a single asserted future (非終末論).
    G2 non-speculative   — use=\":resilience\" (NEVER trade/position/wager — unrepresentable).
    G5 leak-free         — uses ONLY observations strictly before target-at; score-pair RAISES
                           on a look-ahead leak (inherited from score.cljc).
    G12 anti-pseudoscience — skill is pinball vs a documented persistence baseline, not
                           cherry-picked accuracy; skilled only when it beats the baseline.

  A Forecast is the kebab-keyed map produced by score/->forecast; the empirical-quantiles
  baseline is analyze/empirical-quantiles*. Python ':…' keyword strings stay strings.
  Float arithmetic exact: round(x, n) → HALF_EVEN on the exact double. Portable .cljc."
  (:require [mitooshi.methods.analyze :as analyze]
            [mitooshi.methods.score :as score]))

(def default-levels [0.1 0.5 0.9])

;; ── Python round(x, n) — HALF_EVEN on the exact double, returns a double. ─────
#?(:clj
   (defn- py-round-n
     [x n]
     (-> (java.math.BigDecimal. (double x))
         (.setScale (int n) java.math.RoundingMode/HALF_EVEN)
         .doubleValue))
   :cljs
   (defn- py-round-n
     [x n]
     (let [f (Math/pow 10 n)] (/ (Math/round (* (double x) f)) f))))

(defn forecast-next-quantile
  "Forecast series `sid` at `target-at` as empirical QUANTILES of the prior values
  (leak-free — only observations strictly before target-at). nil if no prior history.
  History is a seq of [t v] pairs."
  ([sid history target-at] (forecast-next-quantile sid history target-at default-levels))
  ([sid history target-at levels]
   (let [prior (filterv (fn [[t _v]] (< t target-at)) history)]
     (when (seq prior)
       (let [values (mapv (fn [[_t v]] v) prior)
             info-as-of (reduce max (map first prior))      ; G5
             q (analyze/empirical-quantiles* values levels)]
         (score/->forecast (str "fc." sid "." target-at ".quantile") "quantile"
                           :info-as-of info-as-of :use ":resilience"
                           :point-asserted false :quantiles q))))))

(defn- persistence-quantiles
  "Documented naive baseline: every quantile = the last observed value (no spread).
  A 'tomorrow = today, with certainty' straw man the real forecaster must beat (G12).
  Preserves levels order so pinball-loss iterates them in the same order as Python."
  [values levels]
  (let [last (peek (vec values))]
    (reduce (fn [m tau] (assoc m (double tau) (double last))) {} levels)))

(defn score-quantile
  "Score a quantile forecast against the realizing value (leak-checked by score-pair)."
  [fc y observed-at]
  (score/score-pair fc (score/->observation (str "o." (:fid fc))
                                            :observed-at observed-at :value y)))

(defn forecast-quantile-trail
  "Forecast every series at target-at as quantiles; when the realizing obs is already in the
  trail, score pinball + skill vs the persistence-quantile baseline (G12). `rows` is a seq of
  maps with string keys \":obs/series\" \":obs/observed-at\" \":obs/value\"."
  ([rows target-at] (forecast-quantile-trail rows target-at default-levels))
  ([rows target-at levels]
   (let [;; hist: series -> [[t v] ...] (first-touch order); actual: [series t] -> v
         {:keys [hist h-order actual]}
         (reduce
          (fn [acc r]
            (if (and (contains? r ":obs/series") (contains? r ":obs/observed-at"))
              (let [sid (get r ":obs/series")
                    t (long (get r ":obs/observed-at"))
                    v (double (get r ":obs/value"))]
                (-> acc
                    (update-in [:hist sid] (fnil conj []) [t v])
                    (update :h-order (fn [o] (if (contains? (:hist acc) sid) o (conj o sid))))
                    (assoc-in [:actual [sid t]] v)))
              acc))
          {:hist {} :h-order [] :actual {}}
          rows)
         ;; sorted(hist.items()) — by series id
         sids (sort (keys hist))]
     (reduce
      (fn [out sid]
        (let [h (vec (sort-by (fn [[t v]] [t v]) (get hist sid)))
              fc (forecast-next-quantile sid h target-at levels)]
          (if (nil? fc)
            out
            (let [row {"series" sid "forecast" fc}]
              (if (contains? actual [sid target-at])
                (let [y (get actual [sid target-at])
                      s (score-quantile fc y target-at)       ; raises on G5 leak
                      prior (mapv (fn [[_t v]] v) (filter (fn [[t _v]] (< t target-at)) h))
                      base (score/pinball-loss (persistence-quantiles prior levels) y)
                      row (assoc row
                                 "pinball" (py-round-n (get s "pinball") 6)
                                 "baseline_pinball" (py-round-n base 6)
                                 "skill" (py-round-n (score/skill-score (get s "pinball") base) 4)
                                 "skilled" (boolean (< (get s "pinball") base)))]  ; G12
                  (conj out row))
                (conj out row))))))
      []
      sids))))

;; ── byte-parity self-run (mirrors forecast_quantile.py _run) ─────────────────
(defn run-self-test
  "Mirror of forecast_quantile.py _run: emit a deterministic line for byte-parity cmp."
  []
  (let [hist (mapv (fn [t] [t (double (+ 10 (* 2 t)))]) (range 1 7))
        fc (forecast-next-quantile "s-x" hist 7)]
    (assert (and (some? fc) (= "quantile" (:dist-kind fc)) (false? (:point-asserted fc))))
    (assert (and (= ":resilience" (:use fc)) (= 6 (:info-as-of fc))))
    (let [qs (sort-by key (:quantiles fc))]
      (assert (<= (val (nth qs 0)) (val (nth qs 1)) (val (nth qs 2)))))
    (let [s (score-quantile fc 24.0 7)]
      (assert (and (contains? s "pinball") (contains? s "pit"))))
    (let [rows (mapv (fn [t] {":obs/series" "s-x" ":obs/observed-at" t
                              ":obs/value" (double (+ 10 (* 2 t)))}) (range 1 8))
          trail (forecast-quantile-trail rows 7)]
      (assert (and (seq trail) (contains? (first trail) "skill"))))
    "forecast_quantile.cljc: self-test passed"))

#?(:clj
   (defn -main [& _]
     (println (run-self-test))
     0))
