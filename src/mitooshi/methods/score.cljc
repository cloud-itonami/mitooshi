(ns mitooshi.methods.score
  "mitooshi 見通し — proper-scoring-rule engine. 1:1 Clojure port of methods/score.py
  (ADR-2606051800). The empirical heart of the actor: where FACT meets FORECAST and the
  model error falls out, refusing to score a pairing that would leak future information.

    CRPS    — Continuous Ranked Probability Score (Gaussian closed form + ensemble form)
    pinball — quantile / pinball loss (CRPS for a quantile forecast)
    logscore— negative log predictive density
    brier   — categorical / binary
    PIT     — probability integral transform F(y); Uniform(0,1) iff calibrated
    skill   — 1 − score_model / score_baseline; > 0 iff the model beats the baseline

  CONSTITUTIONAL framing (enforcement-point of the invariants):
    G5 — leak-free: score-pair RAISES (ex-info) if obs.observed-at <= forecast.info-as-of.
    G1 — distribution-only: a forecast with point-asserted=true is rejected at the door.
    G12— anti-pseudoscience: a model is \"skilled\" ONLY if skill > 0 vs a real baseline.

  House style: Python ':…' keyword strings stay strings; pure fns; portable .cljc. Map keys
  in forecast/observation records are kebab keywords (:fid :dist-kind :info-as-of …)."
  (:require [clojure.string :as str]))

(def ^:private sqrt2 (Math/sqrt 2.0))
(def ^:private sqrt-pi (Math/sqrt Math/PI))
(def ^:private inv-sqrt-2pi (/ 1.0 (Math/sqrt (* 2.0 Math/PI))))

;; ── normal helpers ───────────────────────────────────────────────────────
(defn- phi
  "Standard normal pdf."
  [z]
  (* inv-sqrt-2pi (Math/exp (* -0.5 z z))))

;; Clojure has no math.erf, so we reproduce CPython's math.erf to full double precision
;; (last-ULP across the real line, verified vs python3 on a dense grid) via the textbook
;; pairing: a Maclaurin series for |x| < 2 and a complementary-error continued fraction for
;; |x| >= 2. Both converge to machine precision, so the formatted output is byte-identical.
(def ^:private two-over-sqrtpi (/ 2.0 (Math/sqrt Math/PI)))

(defn- erf-series
  "erf(x) = 2/√π · Σ_{n≥0} (−1)^n x^(2n+1) / (n!·(2n+1)); converges fast for |x| < 2."
  [x]
  (loop [n 0, term (double x), sum 0.0]
    (let [add (/ term (+ (* 2.0 n) 1.0))
          sum' (+ sum add)]
      (if (or (> n 200) (< (Math/abs add) (* (Math/abs sum') 1e-18)))
        (* two-over-sqrtpi sum')
        (recur (inc n) (/ (* term (- (* x x))) (+ n 1.0)) sum')))))

(defn- erfc-cf
  "erfc(|x|) via the Lentz-free backward continued fraction (260 terms = full precision)."
  [x]
  (let [ax (Math/abs x)
        x2 (* ax ax)]
    (loop [k 260, acc 0.0]
      (if (zero? k)
        (* (/ (Math/exp (- x2)) (Math/sqrt Math/PI)) (/ 1.0 (+ ax acc)))
        (recur (dec k) (/ (* 0.5 k) (+ ax acc)))))))

(defn- erf
  "Error function, matching python3 math.erf to last ULP across the real line."
  [x]
  (cond
    (zero? x) 0.0
    (< (Math/abs x) 2.0) (erf-series x)
    (>= x 0.0) (- 1.0 (erfc-cf x))
    :else (- (erfc-cf x) 1.0)))

(defn- big-phi
  "Standard normal cdf via erf."
  [z]
  (* 0.5 (+ 1.0 (erf (/ z sqrt2)))))

;; ── proper scoring rules ───────────────────────────────────────────────────
(defn gaussian-crps
  "CRPS of N(mu, sigma^2) at outcome y (Gneiting & Raftery 2007, closed form)."
  [mu sigma y]
  (if (<= sigma 0.0)
    (Math/abs (- y mu))
    (let [z (/ (- y mu) sigma)]
      (* sigma (+ (* z (- (* 2.0 (big-phi z)) 1.0))
                  (* 2.0 (phi z))
                  (- (/ 1.0 sqrt-pi)))))))

(defn gaussian-logscore
  "Negative log predictive density of N(mu, sigma^2) at y. Lower is better."
  [mu sigma y]
  (let [sigma (if (<= sigma 0.0) 1e-9 sigma)]
    (+ (* 0.5 (Math/log (* 2.0 Math/PI sigma sigma)))
       (/ (Math/pow (- y mu) 2) (* 2.0 sigma sigma)))))

(defn gaussian-pit
  "Probability integral transform F(y) for a Gaussian forecast."
  [mu sigma y]
  (if (<= sigma 0.0)
    (if (>= y mu) 1.0 0.0)
    (big-phi (/ (- y mu) sigma))))

(defn pinball-loss
  "Mean pinball / quantile loss over a quantile forecast {level value}.
  Iterates the quantile map in INSERTION order (matches Python dict iteration)."
  [quantiles y]
  (when (empty? quantiles)
    (throw (ex-info "pinball_loss: empty quantile forecast" {})))
  (let [total (reduce (fn [acc [tau q]]
                        (+ acc (if (>= y q) (* (- y q) tau) (* (- q y) (- 1.0 tau)))))
                      0.0 quantiles)]
    (/ total (count quantiles))))

(defn quantile-pit
  "Approximate PIT for a quantile forecast: the level whose value the outcome sits at."
  [quantiles y]
  (let [items (sort-by key quantiles)
        items (vec items)]
    (cond
      (<= y (val (first items))) 0.0
      (>= y (val (peek items))) 1.0
      :else
      (or
       (loop [pairs (map vector items (rest items))]
         (when (seq pairs)
           (let [[[t0 q0] [t1 q1]] (first pairs)]
             (if (and (<= q0 y) (<= y q1))
               (if (== q1 q0)
                 t0
                 (+ t0 (/ (* (- t1 t0) (- y q0)) (- q1 q0))))
               (recur (rest pairs))))))
       0.5))))

(defn ensemble-crps
  "CRPS of an ensemble forecast {x_1..x_m} at outcome y — the empirical (energy) form."
  [members y]
  (let [m (count members)]
    (when (zero? m)
      (throw (ex-info "ensemble_crps: empty ensemble" {})))
    (let [term1 (/ (reduce + 0.0 (map #(Math/abs (- % y)) members)) m)
          term2 (/ (reduce + 0.0 (for [a members b members] (Math/abs (- a b))))
                   (* 2.0 m m))]
      (- term1 term2))))

(defn ensemble-pit
  "PIT-analogue for an ensemble: the fraction of members at or below the outcome."
  [members y]
  (let [m (count members)]
    (when (zero? m)
      (throw (ex-info "ensemble_pit: empty ensemble" {})))
    (/ (double (count (filter #(<= % y) members))) m)))

(defn brier-score
  "Multi-class Brier score: sum_k (p_k - o_k)^2, o_k = 1 for the realized class."
  [probs realized-class]
  (when (empty? probs)
    (throw (ex-info "brier_score: empty categorical forecast" {})))
  (let [classes (conj (set (keys probs)) realized-class)]
    (reduce (fn [acc c]
              (let [o (if (= c realized-class) 1.0 0.0)]
                (+ acc (Math/pow (- (get probs c 0.0) o) 2))))
            0.0 classes)))

(defn categorical-logscore
  "Negative log probability assigned to the realized class. Lower is better."
  [probs realized-class]
  (let [p (max (get probs realized-class 0.0) 1e-12)]
    (- (Math/log p))))

(defn categorical-pit
  "PIT-analogue for a categorical forecast: probability mass at-or-below the realized class
  under the natural ordering of class keys."
  [probs realized-class]
  (let [items (sort-by key probs)]
    (loop [items items, cum 0.0]
      (if (empty? items)
        cum
        (let [[c p] (first items)
              cum (+ cum p)]
          (if (= c realized-class)
            cum
            (recur (rest items) cum)))))))

;; ── baselines (the skill yardstick) ─────────────────────────────────────────
(defn climatology-gaussian
  "Climatology baseline: forecast = N(historical mean, historical sd). Returns [mu sd]."
  [history]
  (when (empty? history)
    (throw (ex-info "climatology needs history" {})))
  (let [n (count history)
        mu (/ (reduce + 0.0 history) n)
        var (/ (reduce + 0.0 (map #(Math/pow (- % mu) 2) history)) (max (- n 1) 1))]
    [mu (if (> var 0) (Math/sqrt var) 1e-9)]))

(defn persistence-gaussian
  "Persistence baseline: forecast = last value, sd = stdev of first differences."
  ([history] (persistence-gaussian history nil))
  ([history spread]
   (when (empty? history)
     (throw (ex-info "persistence needs history" {})))
   (let [mu (last history)]
     (cond
       (some? spread) [mu (max spread 1e-9)]
       (< (count history) 2) [mu 1e-9]
       :else
       (let [v (vec history)
             diffs (mapv #(- (v %) (v (dec %))) (range 1 (count v)))
             md (/ (reduce + 0.0 diffs) (count diffs))
             var (/ (reduce + 0.0 (map #(Math/pow (- % md) 2) diffs)) (max (- (count diffs) 1) 1))]
         [mu (if (> var 0) (Math/sqrt var) 1e-9)])))))

(defn skill-score
  "Skill = 1 - model/baseline. > 0 ⇔ the model beats the baseline (G12)."
  [model-score baseline-score]
  (if (== baseline-score 0.0)
    (if (== model-score 0.0) 0.0 Double/NEGATIVE_INFINITY)
    (- 1.0 (/ model-score baseline-score))))

;; ── the leak-free pair scorer ───────────────────────────────────────────────
;; A Forecast is a map with kebab keys; Observation likewise. Construct via ->forecast /
;; ->observation for the analyze pipeline.
(defn ->forecast
  [fid dist-kind & {:keys [info-as-of use point-asserted mean sd quantiles probs members]
                    :or {use "resilience" point-asserted false mean 0.0 sd 1.0
                         quantiles {} probs {} members []}}]
  {:fid fid :dist-kind dist-kind :info-as-of info-as-of :use use
   :point-asserted point-asserted :mean mean :sd sd
   :quantiles quantiles :probs probs :members members})

(defn ->observation
  [oid & {:keys [observed-at value cls] :or {value 0.0 cls ""}}]
  {:oid oid :observed-at observed-at :value value :cls cls})

(def allowed-use
  #{":resilience" ":planning" ":nowcast" ":early-warning" ":research"
    "resilience" "planning" "nowcast" "early-warning" "research"})

(defn score-pair
  "Score one forecast against the observation that realized it. RAISES on a charter
  violation (G1 point-assertion, G2 illegal use, G5 look-ahead leak)."
  [fc obs]
  ;; G1 — distribution-only.
  (when (:point-asserted fc)
    (throw (ex-info (str "G1: forecast " (pr-str (:fid fc))
                         " asserts a deterministic point; unrepresentable (非終末論)")
                    {:gate "G1" :fid (:fid fc)})))
  ;; G2 — non-speculative use.
  (when-not (contains? allowed-use (:use fc))
    (throw (ex-info (str "G2: forecast " (pr-str (:fid fc)) " use " (pr-str (:use fc))
                         " not in the non-speculative set")
                    {:gate "G2" :fid (:fid fc) :use (:use fc)})))
  ;; G5 — leak-free.
  (when (<= (:observed-at obs) (:info-as-of fc))
    (throw (ex-info (str "G5 LEAK: obs " (pr-str (:oid obs)) " observed_at=" (:observed-at obs)
                         " is not strictly after forecast " (pr-str (:fid fc))
                         " info_as_of=" (:info-as-of fc) "; scoring would see the future")
                    {:gate "G5" :fid (:fid fc) :oid (:oid obs)})))
  (let [dk (:dist-kind fc)]
    (cond
      (contains? #{"gaussian" ":gaussian"} dk)
      {"crps" (gaussian-crps (:mean fc) (:sd fc) (:value obs))
       "log_score" (gaussian-logscore (:mean fc) (:sd fc) (:value obs))
       "pit" (gaussian-pit (:mean fc) (:sd fc) (:value obs))}

      (contains? #{"quantile" ":quantile"} dk)
      {"pinball" (pinball-loss (:quantiles fc) (:value obs))
       "pit" (quantile-pit (:quantiles fc) (:value obs))}

      (contains? #{"categorical" ":categorical"} dk)
      {"brier" (brier-score (:probs fc) (:cls obs))
       "log_score" (categorical-logscore (:probs fc) (:cls obs))
       "pit" (categorical-pit (:probs fc) (:cls obs))}

      (contains? #{"ensemble" ":ensemble"} dk)
      {"crps" (ensemble-crps (:members fc) (:value obs))
       "pit" (ensemble-pit (:members fc) (:value obs))}

      :else
      (throw (ex-info (str "unknown dist_kind " (pr-str dk)) {:dist-kind dk})))))

;; ── calibration over a set of PITs ──────────────────────────────────────────
(defn calibration-summary
  "Reliability of a set of forecasts via their PIT histogram."
  ([pit-values] (calibration-summary pit-values 10))
  ([pit-values bins]
   (if (empty? pit-values)
     {"n" 0 "pit_mean" 0.5 "deviation" 0.0 "hist" []}
     (let [n (count pit-values)
           counts (reduce (fn [cs p]
                            (let [clamped (max 0.0 (min 1.0 p))
                                  idx (min (int (* clamped bins)) (dec bins))]
                              (update cs idx inc)))
                          (vec (repeat bins 0))
                          pit-values)
           freqs (mapv #(/ (double %) n) counts)
           expected (/ 1.0 bins)
           deviation (reduce + 0.0 (map #(Math/abs (- % expected)) freqs))]
       {"n" n
        "pit_mean" (/ (reduce + 0.0 pit-values) n)
        "deviation" deviation
        "hist" freqs}))))

(defn score-set
  "Aggregate a set of leak-checked pairs [[fc obs]…] into a scorecard + calibration +
  (optional) skill vs a parallel baseline score list. The model is `skilled` only if mean
  skill on the primary metric is > 0 (G12). 1:1 port of score_set (completes the score port)."
  ([pairs] (score-set pairs nil))
  ([pairs baseline]
   (let [rows (mapv (fn [[fc obs]] (score-pair fc obs)) pairs)
         agg (reduce (fn [m metric]
                       (let [vals (keep #(get % metric) rows)]
                         (if (seq vals)
                           (assoc m metric (/ (reduce + 0.0 vals) (count vals)))
                           m)))
                     {} ["crps" "pinball" "log_score" "brier"])
         calib (calibration-summary (keep #(get % "pit") rows))
         [skill skilled]
         (if (seq baseline)
           (let [primary (cond (contains? agg "crps") "crps"
                               (contains? agg "pinball") "pinball"
                               (contains? agg "brier") "brier"
                               :else nil)
                 b-vals (when primary (keep #(get % primary) baseline))]
             (if (and primary (seq b-vals))
               (let [sk (skill-score (get agg primary) (/ (reduce + 0.0 b-vals) (count b-vals)))]
                 [sk (> sk 0.0)])
               [nil nil]))
           [nil nil])]
     {"n" (count rows) "metrics" agg "calibration" calib
      "skill" skill "skilled" skilled "rows" rows})))
