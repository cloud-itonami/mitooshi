(ns mitooshi.methods.horizon
  "mitooshi 見通し — multi-horizon skill-decay analysis. 1:1 Clojure port of
  methods/horizon.py (ADR-2606051800).

  A real forecaster predicts at MANY lead times, and its skill decays as the horizon grows —
  eventually a long-range forecast can do no better than climatology. This demonstrates that
  property honestly on a mean-reverting AR(1) process, scoring a leak-free h-step forecaster
  against the climatology baseline at each horizon h = 1..H.

    AR(1):  y_t = μ + φ·(y_{t-1} − μ) + ε_t          (φ = 0.9, mean-reverting)
    optimal h-step mean:  μ + φ^h·(y_t − μ)  →  μ as h→∞
    h-step sd:  σ_ε·sqrt((1 − φ^{2h})/(1 − φ²))  →  the unconditional σ as h→∞

  Distribution-only / never-trades hold by construction: every forecast is a gaussian (a
  distribution, point-asserted=false default), use=\":resilience\"; score-pair RAISES on a
  point assertion (G1), an illegal use (G2), or a look-ahead leak (G5). 非終末論: it never
  claims a flat-skill crystal ball.

  Math fns via Math/ (last-ULP, matching python3 math.* on the same inputs); φ^h via Math/pow
  matching Python's ** on integer exponents. Float formatting matches Python {x:.4f} via
  HALF_EVEN on the exact double. Portable .cljc."
  (:require [clojure.string :as str]
            [mitooshi.methods.score :as score]))

(def mu 10.0)
(def phi 0.9)                       ; strong mean-reversion → clear short-horizon predictability
(def sigma-e 1.0)
(def sigma-uncond (/ sigma-e (Math/sqrt (- 1.0 (* phi phi)))))

(defn- innov
  "Deterministic, non-repeating, ~zero-mean innovation (no RNG — reproducible).
  1.1·sin(2.3t) + 0.7·cos(0.9t+1.0) − 0.5·sin(0.37t)."
  [t]
  (+ (* 1.1 (Math/sin (* 2.3 t)))
     (* 0.7 (Math/cos (+ (* 0.9 t) 1.0)))
     (- (* 0.5 (Math/sin (* 0.37 t))))))

(defn build-path
  "AR(1) path of length n starting at MU."
  [n]
  (loop [t 1, y [mu]]
    (if (>= t n)
      y
      (recur (inc t) (conj y (+ mu (* phi (- (peek y) mu)) (innov t)))))))

(defn- model-forecast
  "Optimal h-step [mean sd] from state y_t. Returns [mean sd]."
  [y-t h]
  (let [mean (+ mu (* (Math/pow phi h) (- y-t mu)))
        var (/ (* sigma-e sigma-e (- 1.0 (Math/pow phi (* 2 h)))) (- 1.0 (* phi phi)))]
    [mean (Math/sqrt var)]))

(defn horizon-skill
  "For each horizon, mean CRPS of the AR(1) forecaster vs the climatology baseline,
  aggregated leak-free over all valid origins. Returns one row (string-keyed map) per horizon."
  ([] (horizon-skill 160 [1 3 6 12]))
  ([n horizons]
   (let [y (build-path n)]
     (mapv
      (fn [h]
        (let [origins (range 2 (- n h))     ; origin t sees y[0..t]; target t+h strictly after
              [model-crps clim-crps]
              (reduce
               (fn [[mc cc] t]
                 (let [target (+ t h)
                       yt (nth y target)
                       [m sd] (model-forecast (nth y t) h)
                       fc (score/->forecast (str "f" t "." h) "gaussian"
                                            :info-as-of t :mean m :sd sd)
                       sc (score/score-pair fc (score/->observation (str "o" target)
                                                                    :observed-at target :value yt))]
                   [(conj mc (get sc "crps"))
                    (conj cc (score/gaussian-crps mu sigma-uncond yt))]))
               [[] []]
               origins)
              n* (count model-crps)
              mc (/ (reduce + 0.0 model-crps) n*)
              cc (/ (reduce + 0.0 clim-crps) n*)]
          {"h" h "mean_crps" mc "clim_crps" cc
           "skill_vs_clim" (score/skill-score mc cc) "n" n*}))
      horizons))))

;; ── float formatting (HALF_EVEN on the exact double, matching Python {x:.4f}) ──
#?(:clj
   (defn- fmt-fixed
     [x n]
     (-> (java.math.BigDecimal. (double x))
         (.setScale (int n) java.math.RoundingMode/HALF_EVEN)
         .toPlainString))
   :cljs
   (defn- fmt-fixed [x n] (.toFixed (double x) n)))

(defn render-md
  "1:1 with horizon.render_md — markdown table, one row per horizon."
  [rows]
  (let [L (transient
           [(str "# mitooshi 見通し — multi-horizon skill decay (AR(1), φ=" phi ")") ""
            "_Skill vs climatology decays as the lead time grows — a long-range forecast eventually_"
            "_does no better than the climatological mean. mitooshi never claims flat-skill foresight (非終末論)._" ""
            "| horizon h | mean CRPS | climatology CRPS | skill vs clim | n |"
            "|---|---|---|---|---|"])]
    (doseq [r rows]
      (conj! L (str "| " (get r "h") " | " (fmt-fixed (get r "mean_crps") 4) " | "
                    (fmt-fixed (get r "clim_crps") 4) " | "
                    (fmt-fixed (get r "skill_vs_clim") 4) " | " (get r "n") " |")))
    (conj! L "")
    (conj! L "→ CRPS rises and skill falls with horizon; the useful-foresight range is where skill > 0.")
    (conj! L "")
    (str/join "\n" (persistent! L))))

#?(:clj
   (defn -main
     "CLI entry: print per-horizon skill (mirrors horizon.main without --out file I/O)."
     [& argv]
     (let [argv (vec argv)
           rows (horizon-skill)]
       (println (str "mitooshi multi-horizon skill (AR(1), φ=" phi "):"))
       (doseq [r rows]
         (println (str "  h=" (get r "h") ": CRPS=" (fmt-fixed (get r "mean_crps") 4)
                       " skill_vs_clim=" (fmt-fixed (get r "skill_vs_clim") 4))))
       (when (some #{"--out"} argv)
         (let [outdir (clojure.java.io/file (nth argv (inc (.indexOf argv "--out"))))]
           (.mkdirs outdir)
           (spit (clojure.java.io/file outdir "horizon-skill.md") (render-md rows))
           (println (str "  → " (clojure.java.io/file outdir "horizon-skill.md")))))
       0)))
