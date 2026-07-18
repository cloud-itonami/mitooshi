(ns mitooshi.methods.forecast
  "mitooshi 見通し — leak-free rolling-origin forecasting + online-recalibrated backtest.
  1:1 port of methods/forecast.py (ADR-2606051800). Composes the score primitives (CRPS / PIT /
  climatology / persistence / skill / calibration, mitooshi.methods.score) into per-series Gaussian
  forecasts and a ROLLING-ORIGIN, leak-free backtest (G5). backtest-calibrated additionally applies
  the online_update cell's learned bias/inflation (apply-correction) using ONLY past residuals.
  The Python __main__ CLI (load-edn + argv dispatch) is the omitted I/O leg; the importlib load of
  the online_update cell becomes a direct require."
  (:require [clojure.string :as str]
            [mitooshi.methods.score :as score]
            [mitooshi.cells.online-update.state-machine :as ou]))

(def methods- ["climatology" "persistence"])

(defn- round4 [x] (/ (Math/round (* (double x) 10000.0)) 10000.0))
(defn- round6 [x] (/ (Math/round (* (double x) 1000000.0)) 1000000.0))

(defn series-histories
  "{series-id [[observed-at value] …] sorted by observed-at} from a trail."
  [rows]
  (->> rows
       (filter #(and (contains? % ":obs/series") (contains? % ":obs/observed-at")))
       (reduce (fn [h r] (update h (get r ":obs/series") (fnil conj [])
                                 [(long (get r ":obs/observed-at")) (double (get r ":obs/value"))])) {})
       (into {} (map (fn [[sid pairs]] [sid (vec (sort-by first pairs))])))))

(defn forecast-next
  "Gaussian forecast of `sid` at target-at using ONLY obs strictly before it (leak-free). nil if no prior."
  ([sid history target-at] (forecast-next sid history target-at "climatology"))
  ([sid history target-at method]
  (when-not (some #{method} methods-)
    (throw (ex-info (str "method must be one of " methods- ", got " (pr-str method)) {})))
  (let [prior (filterv (fn [[t _]] (< t target-at)) history)]
    (when (seq prior)
      (let [values (mapv second prior)
            info-as-of (apply max (map first prior))
            [mu sd] (if (= method "climatology") (score/climatology-gaussian values) (score/persistence-gaussian values))]
        (score/->forecast (str "fc." sid "." target-at "." method) "gaussian"
                          :info-as-of info-as-of :use ":resilience" :point-asserted false
                          :mean (round4 mu) :sd (round6 sd)))))))

(defn forecast-trail
  "Forecast every series at target-at; score leak-free against the realizing obs if present."
  [rows target-at method]
  (let [hist (series-histories rows)
        actual (reduce (fn [m r] (if (and (contains? r ":obs/series") (contains? r ":obs/observed-at"))
                                   (assoc m [(get r ":obs/series") (long (get r ":obs/observed-at"))] (double (get r ":obs/value")))
                                   m)) {} rows)]
    (reduce (fn [out [sid h]]
              (let [fc (forecast-next sid h target-at method)]
                (if (nil? fc) out
                    (let [row {"series" sid "forecast" fc}
                          k [sid target-at]]
                      (conj out
                            (if (contains? actual k)
                              (let [y (get actual k)
                                    s (score/score-pair fc (score/->observation (str "obs." sid "." target-at) :observed-at target-at :value y))
                                    prior (mapv second (filterv (fn [[t _]] (< t target-at)) h))
                                    [cmu csd] (score/climatology-gaussian prior)
                                    base (score/gaussian-crps cmu csd y)]
                                (assoc row "crps" (round6 (get s "crps")) "climatology_crps" (round6 base)
                                       "skill" (round4 (score/skill-score (get s "crps") base))))
                              row))))))
            [] (sort-by first hist))))

(defn backtest-rolling
  "Rolling-origin leak-free backtest: at every origin after the first, forecast + score."
  [rows method]
  (let [hist (series-histories rows)
        targets (vec (sort (set (mapcat (fn [[_ pairs]] (map first pairs)) hist))))
        res (reduce (fn [acc target-at]
                      (let [scored (filterv #(contains? % "crps") (forecast-trail rows target-at method))]
                        (if (empty? scored) acc
                            (let [o-crps (mapv #(get % "crps") scored)
                                  o-skill (mapv #(get % "skill") scored)
                                  pits (mapv (fn [r] (let [sid (get r "series")
                                                           h (get hist sid)
                                                           y (some (fn [[t v]] (when (= t target-at) v)) h)]
                                                       (get (score/score-pair (get r "forecast")
                                                                              (score/->observation "o" :observed-at target-at :value y)) "pit"))) scored)]
                              (-> acc
                                  (update :crps into o-crps) (update :skill into o-skill) (update :pit into pits)
                                  (update :per conj {"target_at" target-at "n" (count scored)
                                                     "mean_crps" (round6 (/ (reduce + o-crps) (count o-crps)))
                                                     "mean_skill" (round4 (/ (reduce + o-skill) (count o-skill)))}))))))
                    {:crps [] :skill [] :pit [] :per []} (rest targets))
        n (count (:crps res))]
    {"method" method "n" n
     "mean_crps" (when (pos? n) (round6 (/ (reduce + (:crps res)) n)))
     "mean_skill" (when (pos? n) (round4 (/ (reduce + (:skill res)) n)))
     "calibration" (score/calibration-summary (:pit res))
     "per_origin" (:per res)}))

(defn compare-methods [rows]
  (into {} (map (fn [m] [m (backtest-rolling rows m)]) methods-)))

(defn recalib-params
  "Batch bias + variance-inflation from PAST residuals (same math as propose-update).
  Returns [bias var-infl]; [0.0 1.0] (identity) when nothing to learn."
  [residuals]
  (let [errs (mapv #(double (get % "error")) (filter #(contains? % "error") residuals))
        sds (mapv #(double (get % "sd")) (filter #(> (or (get % "sd") 0) 0) residuals))]
    (if (empty? errs)
      [0.0 1.0]
      (let [n (count errs)
            mean-err (/ (reduce + errs) n)
            resid-std (if (>= n 2)
                        (let [rv (/ (reduce + (map #(* (- % mean-err) (- % mean-err)) errs)) (- n 1))]
                          (if (> rv 0) (Math/sqrt rv) 0.0))
                        (Math/abs (double (first errs))))
            mean-sd (if (seq sds) (/ (reduce + sds) (count sds)) 1.0)
            raw (if (> mean-sd 0) (/ resid-std mean-sd) 1.0)]
        [(round6 mean-err) (round6 (max 0.25 (min 4.0 raw)))]))))

(defn backtest-calibrated
  "Leak-free ONLINE-recalibrated rolling backtest (apply-correction from past residuals only)."
  [rows method]
  (let [hist (series-histories rows)
        targets (vec (sort (set (mapcat (fn [[_ pairs]] (map first pairs)) hist))))
        sorted-hist (sort-by first hist)
        init {:resid (into {} (map (fn [[sid _]] [sid []]) hist)) :crps [] :skill [] :pit [] :bias {}}
        res (reduce
             (fn [acc target-at]
               (reduce
                (fn [a [sid h]]
                  (let [raw (forecast-next sid h target-at method)]
                    (if (or (nil? raw) (not (some (fn [[t _]] (= t target-at)) h))) a
                        (let [y (some (fn [[t v]] (when (= t target-at) v)) h)
                              [bias infl] (recalib-params (get-in a [:resid sid]))
                              [cmean csd] (ou/apply-correction (:mean raw) (:sd raw) bias infl)
                              corr (score/->forecast (str (:fid raw) ".cal") "gaussian"
                                                     :info-as-of (:info-as-of raw) :use ":resilience"
                                                     :point-asserted false :mean cmean :sd csd)
                              s (score/score-pair corr (score/->observation (str "obs." sid "." target-at) :observed-at target-at :value y))
                              prior (mapv second (filterv (fn [[t _]] (< t target-at)) h))
                              [cmu csd0] (score/climatology-gaussian prior)
                              base (score/gaussian-crps cmu csd0 y)]
                          (-> a
                              (assoc-in [:bias sid] [bias infl])
                              (update :crps conj (get s "crps"))
                              (update :pit conj (get s "pit"))
                              (update :skill conj (score/skill-score (get s "crps") base))
                              (update-in [:resid sid] conj {"error" (- y (:mean raw)) "sd" (:sd raw)}))))))
                acc sorted-hist))
             init (rest targets))
        n (count (:crps res))]
    {"method" method "n" n "calibrated" true
     "mean_crps" (when (pos? n) (round6 (/ (reduce + (:crps res)) n)))
     "mean_skill" (when (pos? n) (round4 (/ (reduce + (:skill res)) n)))
     "calibration" (score/calibration-summary (:pit res))
     "bias_var" (:bias res)}))

(defn emit-scorecard-edn [comparison]
  (let [L (concat
           [";; chokepoint-backtest-scorecard.kotoba.edn — ROLLING-ORIGIN leak-free backtest."
            ";; Aggregate skill vs climatology over ALL origins (no cherry-picked target)."
            ";; G5 leak-free at each origin; G12 skill vs a documented baseline. DERIVED"
            ";; :representative. Live promotion G10-gated. ADR-2606051800." "" "["]
           (map (fn [[m s]] (let [cal (get s "calibration")]
                              (str " {:fc.score/method :" m " :fc.score/n " (get s "n")
                                   " :fc.score/mean-crps " (get s "mean_crps") " :fc.score/mean-skill " (get s "mean_skill")
                                   " :fc.score/pit-mean " (round4 (get cal "pit_mean"))
                                   " :fc.score/calibration-deviation " (round4 (get cal "deviation"))
                                   " :fc.score/sourcing :representative}")))
                (sort-by first comparison))
           ["]"])]
    (str (str/join "\n" L) "\n")))

(defn emit-forecast-edn [forecasts target-at method]
  (let [L (concat
           [(str ";; chokepoint-forecast.kotoba.edn — DISTRIBUTION forecasts @ target=" target-at " (" method ").")
            ";; G1 distribution-only (:forecast/point-asserted false, 非終末論). G5 leak-free"
            ";; (info-as-of < target). DERIVED :representative. Live promotion G10-gated. ADR-2606051800." "" "["]
           (map (fn [row] (let [fc (get row "forecast")]
                            (str " {:forecast/id \"" (:fid fc) "\" :forecast/series \"" (get row "series") "\" "
                                 ":forecast/dist :gaussian :forecast/point-asserted false :forecast/use :resilience "
                                 ":forecast/info-as-of " (:info-as-of fc) " :forecast/target-at " target-at " "
                                 ":forecast/mean " (:mean fc) " :forecast/sd " (:sd fc) " :forecast/sourcing :representative}")))
                forecasts)
           ["]"])]
    (str (str/join "\n" L) "\n")))
