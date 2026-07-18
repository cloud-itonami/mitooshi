(ns mitooshi.cells.backtest-score.state-machine
  "Phase state machine for the mitooshi 見通し backtest_score cell — the G5/G12 scoring reasoner.
  1:1 port of cells/backtest_score/state_machine.py (ADR-2606051800). Runs the leak-checked proper-
  scoring engine (mitooshi.methods.score) over a forecast/observation batch → scorecard, OR REFUSES
  if any pairing would leak (obs not strictly after info-as-of) / asserts a point / uses a bad use.
  REFUSAL gate, not a clamp."
  (:require [clojure.string :as str]
            [mitooshi.methods.score :as score]))

(def state-defaults
  {"phase" "init" "model_id" "" "pairs" [] "baseline_primary" 0.0 "refusal" "" "payload" {}})
(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))
(defn- round6 [x] (/ (Math/round (* (double x) 1000000.0)) 1000000.0))

(defn- mk-forecast [d]
  (score/->forecast (get d "forecastId" (get d "fid" "?"))
                    (norm (get d "distKind" (get d "dist_kind" "gaussian")))
                    :info-as-of (long (get d "infoAsOf" (get d "info_as_of" 0)))
                    :use (norm (get d "use" "resilience"))
                    :point-asserted (boolean (get d "pointAsserted" (get d "point_asserted" false)))
                    :mean (double (get d "mean" 0.0)) :sd (double (get d "sd" 1.0))
                    :quantiles (into {} (map (fn [[k v]] [(double k) (double v)]) (get d "quantiles" {})))
                    :probs (into {} (map (fn [[k v]] [(str k) (double v)]) (get d "probs" {})))
                    :members (mapv double (get d "members" []))))

(defn- mk-obs [d]
  (score/->observation (get d "obsId" (get d "oid" "?"))
                       :observed-at (long (get d "observedAt" (get d "observed_at" 0)))
                       :value (double (get d "value" 0.0)) :cls (get d "class" (get d "cls" ""))))

(defn score-batch [state]
  (let [cs (cell-state state)
        cs (assoc cs "model_id" (get state "model_id" (get cs "model_id"))
                  "pairs" (vec (get state "pairs" (get cs "pairs")))
                  "baseline_primary" (double (get state "baseline_primary" (get cs "baseline_primary"))))
        refuse (fn [msg] {"cell_state" (assoc cs "refusal" msg "phase" "refused")})]
    (let [rows-or-err
          (try
            (reduce (fn [acc p]
                      (conj acc (score/score-pair (mk-forecast (get p "forecast" {})) (mk-obs (get p "obs" {})))))
                    [] (get cs "pairs"))
            (catch clojure.lang.ExceptionInfo e {::err (ex-message e)}))]
      (cond
        (and (map? rows-or-err) (::err rows-or-err)) (refuse (::err rows-or-err))
        (empty? rows-or-err) (refuse "empty batch; nothing to score")
        :else
        (let [rows rows-or-err
              primary (some (fn [m] (when (some #(contains? % m) rows) m)) ["crps" "pinball" "brier"])
              scored (filter #(contains? % primary) rows)
              mean-primary (/ (reduce + (map #(get % primary) scored)) (count scored))
              pit (keep #(get % "pit") rows)
              calib (score/calibration-summary pit)
              skill (when (> (get cs "baseline_primary") 0) (score/skill-score mean-primary (get cs "baseline_primary")))]
          {"cell_state" (assoc cs "phase" "scored" "refusal" ""
                               "payload" {"modelId" (get cs "model_id") "n" (count rows) "metric" primary
                                          "meanScore" (round6 mean-primary)
                                          "pitMean" (round6 (get calib "pit_mean")) "calibDeviation" (round6 (get calib "deviation"))
                                          "skill" (when skill (round6 skill))
                                          "skilled" (boolean (and skill (> skill 0))) "derived" true})})))))

(defn solve [_input-state]
  (throw (ex-info "mitooshi R0 scaffold: activate backtest_score via Council ADR (post-2606051800 ratification)" {:scaffold true})))
