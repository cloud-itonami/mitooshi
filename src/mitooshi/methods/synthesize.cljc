(ns mitooshi.methods.synthesize
  "mitooshi 見通し — cross-actor chokepoint RESILIENCE composite (R1, offline).
  Clojure port of methods/synthesize.py (1:1). ADR-2606051800 / 2606012600.

  Fuse the SAME chokepoint keyword across three actors —
    watari 渡り    → live vessel transit (the :transit-load series),
    watatsuna 綿津綱 → submarine-cable load (the :cable-load series),
    mitooshi 見通し → the forecast next-value distribution,
  into ONE per-chokepoint composite ranked by resilience attention.

  A RESILIENCE map, NEVER a target-list (watari G2 + watatsuna G2): the rank routes
  redundancy / repair pre-staging / congestion-easing — never interdiction.
  Aggregate-first, DERIVED :representative, live promotion G10-gated.

  The composite functions are pure (they take already-loaded row vectors); file I/O
  lives in the caller (analyze/load-edn-file). stdlib only."
  (:require [clojure.string :as str]))

(defn- pyround
  "round(x, n) with banker's rounding (round-half-to-even), matching Python round()."
  [x n]
  (let [f (Math/pow 10.0 n)]
    (/ (Math/rint (* (double x) f)) f)))

(defn chokepoint-of
  "s-malacca-cable / s-malacca-transit → :malacca (as a \":…\" string)."
  [series-id]
  (let [core (if (str/starts-with? series-id "s-") (subs series-id 2) series-id)
        core (reduce (fn [c suf]
                       (if (str/ends-with? c suf)
                         (subs c 0 (- (count c) (count suf)))
                         c))
                     core ["-cable" "-transit"])]
    (str ":" core)))

(defn latest-by-series
  "{series-id → value at the max observed-at} — the current value per series."
  [trail-rows]
  (let [latest (reduce (fn [acc r]
                         (if-not (contains? r ":obs/series")
                           acc
                           (let [sid (get r ":obs/series")
                                 t   (long (get r ":obs/observed-at"))
                                 v   (double (get r ":obs/value"))]
                             (if (or (not (contains? acc sid)) (> t (first (get acc sid))))
                               (assoc acc sid [t v])
                               acc))))
                       {} trail-rows)]
    (reduce-kv (fn [m sid [_ v]] (assoc m sid v)) {} latest)))

(defn forecast-by-series
  "{series-id → [forecast-mean forecast-sd]}."
  [fc-rows]
  (reduce (fn [acc r]
            (if (contains? r ":forecast/series")
              (assoc acc (get r ":forecast/series")
                     [(double (get r ":forecast/mean" 0.0))
                      (double (get r ":forecast/sd" 0.0))])
              acc))
          {} fc-rows))

(defn synthesize
  "Per-chokepoint composite, sorted by resilience attention (desc).

  attention = normalized cable load (capacity-at-risk, the dominant term) + a
  live-pressure bump from current transit, both normalized to [0,1] across
  chokepoints so the blend is scale-free."
  [trail-rows fc-rows]
  (let [cur (latest-by-series trail-rows)
        fc  (forecast-by-series fc-rows)
        chokes (reduce
                (fn [acc [sid v]]
                  (let [cp (chokepoint-of sid)
                        d  (get acc cp {"chokepoint" cp "transit" nil
                                        "cable_load" nil "forecast_cable_mean" nil})
                        d  (cond
                             (str/ends-with? sid "-transit")
                             (assoc d "transit" v
                                    "forecast_transit_mean" (first (get fc sid [nil nil])))
                             (str/ends-with? sid "-cable")
                             (assoc d "cable_load" v
                                    "forecast_cable_mean" (first (get fc sid [nil nil])))
                             :else d)]
                    (assoc acc cp d)))
                {} cur)
        ds        (vals chokes)
        cables    (keep #(get % "cable_load") ds)
        transits  (keep #(get % "transit") ds)
        max-cable    (if (seq cables) (apply max cables) 1.0)
        max-transit  (if (seq transits) (apply max transits) 1.0)
        scored (map (fn [d]
                      (let [cl (get d "cable_load")
                            tr (get d "transit")
                            nc (if (and cl (not (zero? max-cable))) (/ cl max-cable) 0.0)
                            nt (if (and tr (not (zero? max-transit))) (/ tr max-transit) 0.0)]
                        ;; cable load dominates (capacity-at-risk); transit is secondary
                        (assoc d "attention" (pyround (+ (* 0.7 nc) (* 0.3 nt)) 4))))
                    ds)]
    (vec (sort-by #(- (get % "attention")) scored))))

(defn- n*
  "Render a value or nil as its EDN token (mirror of the Python `_n` helper)."
  [x]
  (if (nil? x) "nil" (str x)))

(defn render-edn
  "The composite as a kotoba EDN string (cross-actor watari+watatsuna+mitooshi)."
  [composite]
  (let [head [";; chokepoint-resilience-composite.kotoba.edn — cross-actor (watari+watatsuna+mitooshi)."
              ";; ONE maritime resilience picture per chokepoint: live transit + cable load +"
              ";; forecast. attention = 0.7*norm(cable) + 0.3*norm(transit). A RESILIENCE map,"
              ";; NEVER a target-list (routed to redundancy/repair, never interdiction)."
              ";; DERIVED :representative. Live promotion G10-gated. ADR-2606012600." "" "["]
        rows (map (fn [d]
                    (str " {:choke/id " (get d "chokepoint")
                         " :choke/transit " (n* (get d "transit"))
                         " :choke/cable-load-tbps " (n* (get d "cable_load"))
                         " :choke/forecast-cable-mean " (n* (get d "forecast_cable_mean"))
                         " :choke/attention " (get d "attention")
                         " :choke/sourcing :representative}"))
                  composite)]
    (str (str/join "\n" (concat head rows ["]"])) "\n")))
