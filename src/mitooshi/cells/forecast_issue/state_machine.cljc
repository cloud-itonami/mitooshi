(ns mitooshi.cells.forecast-issue.state-machine
  "Phase state machine for the mitooshi 見通し forecast_issue cell — the G1/G2 structural-invariant gate.
  1:1 port of cells/forecast_issue/state_machine.py (ADR-2606051800). A forecast is ISSUED only if
  G1 point_asserted false (no deterministic single-future, 非終末論), G2 use ∈ non-speculative set,
  and the distribution is well-formed (gaussian sd>0, quantile/categorical non-empty + normalized).
  REFUSAL gate, not a clamp."
  (:require [clojure.string :as str]))

(def allowed-use #{"resilience" "planning" "nowcast" "early-warning" "research"})
(def dist-kinds #{"gaussian" "quantile" "categorical" "ensemble"})

(def state-defaults
  {"phase" "init" "forecast_id" "" "series_id" "" "dist_kind" "gaussian" "use" "resilience"
   "point_asserted" false "info_as_of" 0 "horizon" 1 "mean" 0.0 "sd" 1.0
   "quantiles" {} "probs" {} "refusal" "" "payload" {}})
(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))
(defn- to-int [v] (long (or v 0)))

(defn issue [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "forecast_id" (get state "forecast_id" (get cs "forecast_id"))
                  "series_id" (get state "series_id" (get cs "series_id"))
                  "dist_kind" (norm (get state "dist_kind" (get cs "dist_kind")))
                  "use" (norm (get state "use" (get cs "use")))
                  "point_asserted" (boolean (get state "point_asserted" (get cs "point_asserted")))
                  "info_as_of" (to-int (get state "info_as_of" (get cs "info_as_of")))
                  "horizon" (to-int (get state "horizon" (get cs "horizon")))
                  "mean" (double (get state "mean" (get cs "mean")))
                  "sd" (double (get state "sd" (get cs "sd")))
                  "quantiles" (into {} (get state "quantiles" (get cs "quantiles")))
                  "probs" (into {} (get state "probs" (get cs "probs"))))
        refuse (fn [msg] {"cell_state" (assoc cs "refusal" msg "phase" "refused")})
        dk (get cs "dist_kind")]
    (cond
      (get cs "point_asserted")
      (refuse (str "G1: forecast " (pr-str (get cs "forecast_id")) " asserts a deterministic point; unrepresentable (非終末論)"))
      (not (contains? allowed-use (get cs "use")))
      (refuse (str "G2: use " (pr-str (get cs "use")) " not in the non-speculative set "
                   (pr-str (vec (sort allowed-use))) "; mitooshi never trades"))
      (not (contains? dist-kinds dk))
      (refuse (str "unknown dist-kind " (pr-str dk)))
      (and (= dk "gaussian") (not (> (get cs "sd") 0)))
      (refuse (str "gaussian forecast " (pr-str (get cs "forecast_id")) " needs sd > 0 (a degenerate spike is a point assertion)"))
      (and (= dk "quantile") (empty? (get cs "quantiles")))
      (refuse (str "quantile forecast " (pr-str (get cs "forecast_id")) " has no quantiles"))
      (and (= dk "categorical") (empty? (get cs "probs")))
      (refuse (str "categorical forecast " (pr-str (get cs "forecast_id")) " has no class probabilities"))
      (and (= dk "categorical") (> (Math/abs (- (reduce + (vals (get cs "probs"))) 1.0)) 1e-6))
      (refuse (str "categorical forecast " (pr-str (get cs "forecast_id")) " probabilities sum to "
                   (format "%.4f" (double (reduce + (vals (get cs "probs"))))) ", not 1"))
      :else
      {"cell_state" (assoc cs "phase" "issued" "refusal" ""
                           "payload" {"forecastId" (get cs "forecast_id") "seriesId" (get cs "series_id")
                                      "distKind" dk "use" (get cs "use") "pointAsserted" false
                                      "infoAsOf" (get cs "info_as_of") "targetAt" (+ (get cs "info_as_of") (get cs "horizon"))
                                      "horizon" (get cs "horizon") "issued" true})})))

(defn solve [_input-state]
  (throw (ex-info "mitooshi R0 scaffold: activate forecast_issue via Council ADR (post-2606051800 ratification)" {:scaffold true})))
