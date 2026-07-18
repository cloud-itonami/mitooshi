(ns mitooshi.methods.analyze
  "mitooshi 見通し — forecasting-observatory backtest analyzer.
  1:1 Clojure port of `methods/analyze.py` (ADR-2606051800).

  Reads a kotoba-EDN forecasting graph (:series/* :obs/* :forecast/* :fc.model/*
  :baseline/*) and emits:

    1. an aggregate-first scorecard (out/scorecard.md): per-model mean CRPS / log-score,
       calibration (PIT mean + deviation), and SKILL vs the climatology + persistence
       baselines — the honest answer to \"how wrong is the model, measured against fact\".
    2. the derived score datoms (out/forecast-scorecard.kotoba.edn), flagged :derived —
       never re-ingested as authoritative fact.

  It joins FACT to FORECAST leak-free: each forecast is scored only against the observation
  whose :obs/observed-at is STRICTLY AFTER the forecast's :info-as-of (methods/score.cljc
  raises otherwise). Baselines are built using ONLY observations the forecaster could see
  (observed-at ≤ info-as-of), so the skill comparison is itself leak-free.

  CONSTITUTIONAL gates (read before any change):
    G1 distribution-only — a forecast with :forecast/point-asserted true is rejected at the
      door (score/score-pair raises). A deterministic single-future is unrepresentable.
    G2 non-speculative — :forecast/use enum excludes trade/speculation/wager/position;
      mitooshi NEVER TRADES, settles no money, holds no position.
    G5 leak-free proper-scoring — score-pair RAISES if the obs is not strictly after the
      forecast's :info-as-of. Proper scoring rules ONLY; skill vs a documented baseline.
    G12 anti-pseudoscience — :skilled is true ONLY when the model beats a baseline on a
      proper score.

  House style: Python ':…' keyword strings stay strings (incl. all :series/* / :forecast/*
  attrs); pure fns; file I/O only at #?(:clj) edges. Float formatting matches Python's
  {x:.Nf} / round() exactly via HALF_EVEN on the exact BigDecimal of the double. Portable .cljc."
  (:require [clojure.string :as str]
            [mitooshi.methods.score :as score]))

;; ── minimal EDN reader (subset: [] {} :kw \"str\" num bool nil) — ported from analyze.py's
;; _TOK / _tokens / _atom / _parse / _parse_from. Keywords are kept as \":ns/name\" strings
;; (NOT clojure keywords) so the whole pipeline stays string-keyed, byte-for-byte the same
;; as the Python port. Maps preserve insertion order (mirroring Python dict order).

(def ^:private tok-re
  ;; _TOK = re.compile(r'[\s,]+|;[^\n]*|(\[|\]|\{|\}|"(?:\\.|[^"\\])*"|[^\s,\[\]{}]+)')
  #"[\s,]+|;[^\n]*|(\[|\]|\{|\}|\"(?:\\.|[^\"\\])*\"|[^\s,\[\]{}]+)")

(defn tokens
  "Lazy seq of significant tokens (group 1 of each tok-re match that captured)."
  [s]
  (let [m (re-matcher tok-re s)]
    ((fn step []
       (lazy-seq
        (when (.find m)
          (let [t (.group m 1)]
            (if (nil? t)
              (step)
              (cons t (step))))))))))

(defn atom-of
  "Port of _atom: \"…\" → unescaped string; true/false/nil → bool/nil; \":…\" kept as string;
  int → long; else float; else raw string."
  [t]
  (cond
    (str/starts-with? t "\"")
    (-> (subs t 1 (dec (count t)))
        (str/replace "\\\"" "\"")
        (str/replace "\\\\" "\\"))
    (= t "true") true
    (= t "false") false
    (= t "nil") nil
    (str/starts-with? t ":") t
    :else
    (let [as-long (try (Long/parseLong t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
      (if (not= as-long ::nan)
        as-long
        (let [as-dbl (try (Double/parseDouble t) (catch #?(:clj Exception :cljs :default) _ ::nan))]
          (if (not= as-dbl ::nan) as-dbl t))))))

;; analyze.py's _parse / _parse_from read forms from an iterator; we work over a token
;; vector + index. parse-form consumes one form at index i; for `[`/`{` it loops to the
;; closing delimiter. Insertion order of map keys is preserved via an ordered carrier
;; (array-map ≤8 keys; ::order meta tracks order for any size).
(defn- ordered-assoc [m k v]
  (let [had? (contains? m k)
        m' (assoc m k v)]
    (if had?
      (with-meta m' (meta m))
      (with-meta m' (update (meta m) ::order (fnil conj []) k)))))

(declare parse-form)

(defn- parse-seq
  "Consume forms until the closing delim `close` (\"]\" or \"}\"); returns [coll next-i]."
  [toks i close build]
  (loop [i i, acc (build)]
    (let [t (nth toks i)]
      (if (= t close)
        [acc (inc i)]
        (let [[x i] (parse-form toks i)]
          (recur i (conj acc x)))))))

(defn- parse-form
  "Consume one form from the token vector at index i. Returns [value next-i]."
  [toks i]
  (let [t (nth toks i)]
    (cond
      (= t "[")
      (parse-seq toks (inc i) "]" (constantly []))

      (= t "{")
      (loop [i (inc i), m (with-meta {} {::order []})]
        (let [tk (nth toks i)]
          (if (= tk "}")
            [m (inc i)]
            (let [[k i] (parse-form toks i)
                  [v i] (parse-form toks i)]
              (recur i (ordered-assoc m k v))))))

      :else
      [(atom-of t) (inc i)])))

(defn read-edn
  "Parse the first top-level form from EDN text (matches load_edn → _parse(_tokens(text)))."
  [text]
  (let [toks (vec (tokens text))]
    (first (parse-form toks 0))))

(defn- ordered-keys
  "Keys of a parsed map in insertion order (::order meta if present, else seq order)."
  [m]
  (or (::order (meta m)) (keys m)))

;; ── metric-aware baselines ───────────────────────────────────────────────
(defn empirical-quantiles*
  "_empirical_quantiles: linear-interpolated empirical quantiles at the given levels.
  Returns a map {level value}."
  [history levels]
  (let [h (vec (sort history))
        n (count h)]
    (reduce
     (fn [out tau]
       (if (= n 1)
         (assoc out tau (h 0))
         (let [idx (* tau (- n 1))
               lo (int idx)
               hi (min (+ lo 1) (- n 1))]
           (assoc out tau (+ (h lo) (* (- idx lo) (- (h hi) (h lo))))))))
     {} levels)))

(defn class-freqs
  "_class_freqs: empirical class frequencies {class freq} (counts in first-appearance
  order, mirroring collections.Counter; order is immaterial to brier_score)."
  [classes]
  (let [n (count classes)
        counts (reduce (fn [m c] (update m c (fnil inc 0)))
                       (with-meta {} {::order []}) classes)
        counts (reduce (fn [m c] (if (contains? m c) m (ordered-assoc m c (get counts c))))
                       (with-meta {} {::order []})
                       classes)]
    ;; rebuild as {k (v/n)} preserving first-touch order
    (reduce (fn [out k] (assoc out k (/ (double (get counts k)) n)))
            {} (ordered-keys counts))))

;; ── backtest (gaussian / quantile / categorical / ensemble) ─────────────────
(defn- lstrip-colon [s] (if (and (string? s) (str/starts-with? s ":")) (subs s 1) s))

(defn- f->double [v] (double v))

(defn backtest
  "Port of analyze.backtest. Returns {\"series\" {sid rec} \"models\" {mid rec} \"cards\" [..]}.
  series + models preserve insertion order; cards are sorted by model id (Python
  sorted(per_model.items()))."
  [records]
  (let [records (vec records)
        series (reduce (fn [m r] (if (contains? r ":series/id")
                                   (ordered-assoc m (get r ":series/id") r) m))
                       (with-meta {} {::order []}) records)
        obs (filterv #(contains? % ":obs/id") records)
        forecasts (filterv #(contains? % ":forecast/id") records)
        models (reduce (fn [m r] (if (contains? r ":fc.model/id")
                                   (ordered-assoc m (get r ":fc.model/id") r) m))
                       (with-meta {} {::order []}) records)
        ;; index full observations by series, sorted by observed-at (stable)
        by-series (reduce (fn [m o] (update m (get o ":obs/series") (fnil conj []) o))
                          {} obs)
        by-series (reduce-kv (fn [m k v]
                               (assoc m k (vec (sort-by #(get % ":obs/observed-at") v))))
                             {} by-series)
        ;; fold the forecasts into per-model accumulators (insertion order of first touch)
        per-model
        (reduce
         (fn [pm fc]
           (let [sid (get fc ":forecast/series")
                 mid (get fc ":forecast/model" "?")
                 info (get fc ":forecast/info-as-of")
                 target (get fc ":forecast/target-at")
                 dk (lstrip-colon (get fc ":forecast/dist-kind"))
                 use (lstrip-colon (get fc ":forecast/use" ":resilience"))
                 point (boolean (get fc ":forecast/point-asserted" false))
                 hit (first (filter #(= (get % ":obs/observed-at") target) (get by-series sid [])))]
             (if (nil? hit)
               pm
               (let [seen (filterv #(<= (get % ":obs/observed-at") info) (get by-series sid []))
                     m0 (get pm mid {"dist" dk "metric" "" "primary" [] "logscore" [] "pit" []
                                     "base_clim" [] "base_persist" [] "n" 0})
                     [m sc]
                     (cond
                       (= dk "gaussian")
                       (let [y (get hit ":obs/value")
                             f (score/->forecast (get fc ":forecast/id") "gaussian"
                                                 :info-as-of info
                                                 :mean (f->double (get fc ":forecast/mean"))
                                                 :sd (f->double (get fc ":forecast/sd"))
                                                 :use use :point-asserted point)
                             sc (score/score-pair f (score/->observation (str "obs@" target)
                                                                          :observed-at target :value y))
                             hist (mapv #(get % ":obs/value") seen)
                             m (-> m0
                                   (assoc "metric" "CRPS")
                                   (update "primary" conj (get sc "crps"))
                                   (update "logscore" conj (get sc "log_score")))
                             m (if (>= (count hist) 2)
                                 (let [[cmu csd] (score/climatology-gaussian hist)
                                       [pmu psd] (score/persistence-gaussian hist)]
                                   (-> m
                                       (update "base_clim" conj (score/gaussian-crps cmu csd y))
                                       (update "base_persist" conj (score/gaussian-crps pmu psd y))))
                                 m)]
                         [m sc])

                       (= dk "quantile")
                       (let [y (get hit ":obs/value")
                             q (reduce (fn [acc k] (assoc acc (f->double k)
                                                          (f->double (get (get fc ":forecast/quantiles") k))))
                                       {} (ordered-keys (get fc ":forecast/quantiles")))
                             f (score/->forecast (get fc ":forecast/id") "quantile"
                                                 :info-as-of info :quantiles q
                                                 :use use :point-asserted point)
                             sc (score/score-pair f (score/->observation (str "obs@" target)
                                                                          :observed-at target :value y))
                             hist (mapv #(get % ":obs/value") seen)
                             m (-> m0
                                   (assoc "metric" "pinball")
                                   (update "primary" conj (get sc "pinball")))
                             m (if (>= (count hist) 2)
                                 (update m "base_clim" conj
                                         (score/pinball-loss (empirical-quantiles* hist (keys q)) y))
                                 m)]
                         [m sc])

                       (= dk "categorical")
                       (let [cls (get hit ":obs/class" "")
                             probs (reduce (fn [acc k] (assoc acc (str k)
                                                             (f->double (get (get fc ":forecast/probs") k))))
                                           {} (ordered-keys (get fc ":forecast/probs")))
                             f (score/->forecast (get fc ":forecast/id") "categorical"
                                                 :info-as-of info :probs probs
                                                 :use use :point-asserted point)
                             sc (score/score-pair f (score/->observation (str "obs@" target)
                                                                          :observed-at target :cls cls))
                             histc (filterv some? (map #(get % ":obs/class") (filter #(contains? % ":obs/class") seen)))
                             m (-> m0
                                   (assoc "metric" "Brier")
                                   (update "primary" conj (get sc "brier"))
                                   (update "logscore" conj (get sc "log_score")))
                             m (if (seq histc)
                                 (update m "base_clim" conj (score/brier-score (class-freqs histc) cls))
                                 m)]
                         [m sc])

                       (= dk "ensemble")
                       (let [y (get hit ":obs/value")
                             members (mapv f->double (get fc ":forecast/members"))
                             f (score/->forecast (get fc ":forecast/id") "ensemble"
                                                 :info-as-of info :members members
                                                 :use use :point-asserted point)
                             sc (score/score-pair f (score/->observation (str "obs@" target)
                                                                          :observed-at target :value y))
                             hist (mapv #(get % ":obs/value") seen)
                             m (-> m0
                                   (assoc "metric" "CRPS")
                                   (update "primary" conj (get sc "crps")))
                             m (if (>= (count hist) 2)
                                 (update m "base_clim" conj (score/ensemble-crps hist y))
                                 m)]
                         [m sc])

                       :else [nil nil])]
                 (if (nil? m)
                   pm
                   (assoc pm mid (-> m
                                     (update "pit" conj (get sc "pit"))
                                     (update "n" inc))))))))
         {}
         forecasts)
        cards
        (mapv
         (fn [mid]
           (let [m (get per-model mid)
                 n (get m "n")
                 mean-primary (/ (reduce + 0.0 (get m "primary")) n)
                 ls (get m "logscore")
                 mean-ls (when (seq ls) (/ (reduce + 0.0 ls) (count ls)))
                 calib (score/calibration-summary (get m "pit"))
                 bc (get m "base_clim")
                 bp (get m "base_persist")
                 skill-clim (when (seq bc)
                              (score/skill-score mean-primary (/ (reduce + 0.0 bc) (count bc))))
                 skill-persist (when (seq bp)
                                 (score/skill-score mean-primary (/ (reduce + 0.0 bp) (count bp))))
                 skilled (if (= (get m "dist") "gaussian")
                           (boolean (and skill-clim (> skill-clim 0)
                                         skill-persist (> skill-persist 0)))
                           (boolean (and (some? skill-clim) (> skill-clim 0))))]
             {"model" mid
              "name" (get (get models mid {}) ":fc.model/name" mid)
              "dist" (get m "dist")
              "metric" (get m "metric")
              "n" n
              "mean_primary" mean-primary
              "mean_logscore" mean-ls
              "pit_mean" (get calib "pit_mean")
              "calib_deviation" (get calib "deviation")
              "pit_hist" (get calib "hist")
              "skill_vs_climatology" skill-clim
              "skill_vs_persistence" skill-persist
              "skilled" skilled}))
         (sort (ordered-keys per-model)))]
    {"series" series "models" models "cards" cards}))

;; ── float formatting (HALF_EVEN on the exact double, matching Python {x:.Nf} / round) ──
#?(:clj
   (defn- fmt-fixed
     "Python f\"{x:.Nf}\" — fixed-point with HALF_EVEN rounding on the exact double value."
     [x n]
     (-> (java.math.BigDecimal. (double x))
         (.setScale (int n) java.math.RoundingMode/HALF_EVEN)
         .toPlainString))
   :cljs
   (defn- fmt-fixed [x n] (.toFixed (double x) n)))

#?(:clj
   (defn- py-round
     "Python round(x) → nearest integer, HALF_EVEN, as a long."
     [x]
     (-> (java.math.BigDecimal. (double x))
         (.setScale 0 java.math.RoundingMode/HALF_EVEN)
         .longValueExact))
   :cljs
   (defn- py-round [x] (Math/round (double x))))

(defn- fmt-width
  "Python f\"{x:4.0f}\" — width 4, 0 decimals, HALF_EVEN, right-justified, space-padded."
  [x width]
  (let [s (fmt-fixed x 0)]
    (str (apply str (repeat (max 0 (- width (count s))) " ")) s)))

(defn- _fmt [x] (if (nil? x) "n/a" (fmt-fixed x 4)))

;; ── render ───────────────────────────────────────────────────────────────
(defn render-md
  "1:1 with analyze.render_md."
  [res]
  (let [L (transient
           ["# mitooshi 見通し — forecasting scorecard" ""
            "_Leak-free proper-scoring backtest. Lower CRPS / log-score = better; skill > 0 = beats the baseline._"
            "_All figures :representative (G11). 非終末論: this is a moving record, not a final verdict._" ""])
        series (get res "series")]
    (doseq [sid (ordered-keys series)]
      (let [s (get series sid)]
        (conj! L (str "- **series** `" (get s ":series/id") "` — " (get s ":series/name" "") " "
                      "(" (get s ":series/kind" "") ", " (get s ":series/unit" "") ", source-class "
                      (get s ":series/source-class" "") ")"))))
    (conj! L "")
    (conj! L "## Per-model scorecard")
    (conj! L "")
    (conj! L "| model | dist | n | metric | mean score | PIT mean | calib dev | skill vs clim | skill vs persist | skilled? |")
    (conj! L "|---|---|---|---|---|---|---|---|---|---|")
    (doseq [c (get res "cards")]
      (conj! L (str "| " (get c "name") " | " (get c "dist") " | " (get c "n") " | " (get c "metric") " | "
                    (_fmt (get c "mean_primary")) " | "
                    (_fmt (get c "pit_mean")) " | " (_fmt (get c "calib_deviation")) " | "
                    (_fmt (get c "skill_vs_climatology")) " | "
                    (_fmt (get c "skill_vs_persistence")) " | "
                    (if (get c "skilled") "✅" "❌ (honest)") " |")))
    (conj! L "")
    (conj! L "## Reading this")
    (conj! L "- **mean score** is a PROPER scoring rule (CRPS / pinball / Brier) — the distance between the forecast distribution and the realized fact; lower = better; it is the model error.")
    (conj! L "- **PIT mean ≈ 0.5 + low calib-deviation** = the forecast's stated uncertainty matches reality (calibrated).")
    (conj! L "- **skilled** is true ONLY when the model beats BOTH climatology and persistence (G12). An honest ❌")
    (conj! L "  means: keep the baseline; do not promote (calibration_gate would refuse, G7/G12).")
    (conj! L "- The residuals feeding online_update are exactly `y − mean` per forecast; that is what corrects the weights.")
    (conj! L "")
    (str/join "\n" (persistent! L))))

(defn render-datoms
  "1:1 with analyze.render_datoms."
  [res]
  (let [L (transient
           [";; forecast-scorecard.kotoba.edn — DERIVED (:fc.score/derived true). Do NOT re-ingest as fact."
            ";; ADR-2606051800 · generated by methods/analyze.py" "" "["])]
    (doseq [c (get res "cards")]
      (conj! L (str " {:fc.score/id \"score-" (get c "model") "\" :fc.score/model \"" (get c "model") "\" "
                    ":fc.score/metric \"" (get c "metric") "\" :fc.score/value " (fmt-fixed (get c "mean_primary") 6) " "
                    ":fc.score/pit " (fmt-fixed (get c "pit_mean") 6) " "
                    ":fc.score/skill " (if (nil? (get c "skill_vs_climatology"))
                                         "nil" (fmt-fixed (get c "skill_vs_climatology") 6)) " "
                    ":fc.model/skilled " (if (get c "skilled") "true" "false") " :fc.score/derived true}")))
    (conj! L "]")
    (str (str/join "\n" (persistent! L)) "\n")))

(defn render-reliability
  "1:1 with analyze.render_reliability — text reliability diagram per model."
  [res]
  (let [L (transient
           ["# mitooshi 見通し — reliability diagrams (PIT calibration)" ""
            "_PIT ~ Uniform(0,1) ⇔ calibrated. Each `#` ≈ 2% of mass; `·` marks the 10% uniform ideal._"
            "_非終末論: a moving record (G7). All figures :representative._"
            "_HONEST small-sample caveat: each model here has only 3–6 PIT points over 10 bins, so the_"
            "_histogram is necessarily lumpy and `deviation` is inflated — a calibration verdict needs_"
            "_a far larger sample (R1, live-gated). The PIT MEAN (≈0.5 ⇔ unbiased) is the reliable signal here._" ""])]
    (doseq [c (get res "cards")]
      (let [hist (or (get c "pit_hist") [])]
        (conj! L (str "## " (get c "name") " (" (get c "dist") ") — PIT mean " (fmt-fixed (get c "pit_mean") 3)
                      ", deviation " (fmt-fixed (get c "calib_deviation") 3)))
        (conj! L "")
        (let [ideal (if (seq hist) (/ 1.0 (count hist)) 0.1)
              ideal-cells (py-round (* ideal 50))]
          (doseq [[i f] (map-indexed vector hist)]
            (let [lo (/ (double i) (count hist))
                  hi (/ (double (+ i 1)) (count hist))
                  bar-n (py-round (* f 50))
                  bar (apply str (repeat bar-n "#"))
                  bar (if (<= ideal-cells bar-n)
                        (if (> bar-n ideal-cells)
                          (str (subs bar 0 ideal-cells) "·" (subs bar (+ ideal-cells 1)))
                          (str bar "·"))
                        (str bar (apply str (repeat (- ideal-cells bar-n) " ")) "·"))]
              (conj! L (str "`[" (fmt-fixed lo 1) "–" (fmt-fixed hi 1) ")` " bar " " (fmt-width (* f 100) 4) "%")))))
        (let [verdict (if (<= (get c "calib_deviation") 0.4)
                        "calibrated"
                        "MISCALIBRATED → calibration_gate would refuse (G7)")]
          (conj! L "")
          (conj! L (str "→ " verdict))
          (conj! L ""))))
    (str/join "\n" (persistent! L))))

(defn render-reliability-datoms
  "1:1 with analyze.render_reliability_datoms."
  [res]
  (let [L (transient
           [";; reliability.kotoba.edn — DERIVED PIT calibration (:fc.calib/*). Do NOT re-ingest as fact."
            ";; ADR-2606051800 · generated by methods/analyze.py" "" "["])]
    (doseq [c (get res "cards")]
      (let [hist (str/join " " (map #(fmt-fixed % 4) (or (get c "pit_hist") [])))]
        (conj! L (str " {:fc.calib/id \"calib-" (get c "model") "\" :fc.calib/model \"" (get c "model") "\" "
                      ":fc.calib/pit-mean " (fmt-fixed (get c "pit_mean") 6) " :fc.calib/deviation "
                      (fmt-fixed (get c "calib_deviation") 6) " "
                      ":fc.calib/hist \"[" hist "]\"}"))))
    (conj! L "]")
    (str (str/join "\n" (persistent! L)) "\n")))

#?(:clj
   (defn load-edn-file
     "Read + parse a forecasting EDN graph file → records. File I/O only at this edge."
     [path]
     (read-edn (slurp (str path)))))

#?(:clj
   (defn -main
     "CLI entry: analyze a seed EDN graph → out/{scorecard.md,forecast-scorecard.kotoba.edn,
     reliability.md,reliability.kotoba.edn} (file I/O at the edge)."
     [& argv]
     (let [argv (vec argv)
           here (-> *file* clojure.java.io/file .getParentFile)
           seed (if (and (seq argv) (not (str/starts-with? (first argv) "--")))
                  (clojure.java.io/file (first argv))
                  (clojure.java.io/file (.getParentFile here) "data" "seed-forecast-graph.kotoba.edn"))
           outdir (if (some #{"--out"} argv)
                    (clojure.java.io/file (nth argv (inc (.indexOf argv "--out"))))
                    (clojure.java.io/file here "out"))
           records (load-edn-file seed)
           res (backtest records)]
       (.mkdirs outdir)
       (spit (clojure.java.io/file outdir "scorecard.md") (render-md res))
       (spit (clojure.java.io/file outdir "forecast-scorecard.kotoba.edn") (render-datoms res))
       (spit (clojure.java.io/file outdir "reliability.md") (render-reliability res))
       (spit (clojure.java.io/file outdir "reliability.kotoba.edn") (render-reliability-datoms res))
       (println (str "mitooshi: scored " (reduce + 0 (map #(get % "n") (get res "cards")))
                     " forecast(s) across " (count (get res "cards")) " model(s)"))
       (doseq [c (get res "cards")]
         (println (str "  " (get c "name") " [" (get c "dist") "]: " (get c "metric") "="
                       (fmt-fixed (get c "mean_primary") 4)
                       " skill_vs_clim=" (_fmt (get c "skill_vs_climatology"))
                       " skill_vs_persist=" (_fmt (get c "skill_vs_persistence"))
                       " skilled=" (if (get c "skilled") "True" "False"))))
       (println (str "  → " (clojure.java.io/file outdir "scorecard.md") " + "
                     (clojure.java.io/file outdir "reliability.md")))
       0)))
