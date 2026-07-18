(ns mitooshi.cells.online-update.state-machine
  "Phase state machine for the mitooshi 見通し online_update cell — the weight-correction step.
  1:1 port of cells/online_update/state_machine.py (ADR-2606051800). Facts → residuals → recalibrate:
  bias_corr = EWMA(prior, mean residual) corrects systematic drift; var_infl = resid_std / mean_sd
  (EWMA'd + clamped) recalibrates spread toward a uniform PIT. PROPOSES a new version; never promotes
  (calibration_gate does, G9). G8: training runtime must be :baien-edge (no commercial GPU)."
  (:require [clojure.string :as str]))

(def state-defaults
  {"phase" "init" "model_id" "" "from_version" 1 "alpha" 0.3 "prior_bias" 0.0 "prior_var_infl" 1.0
   "residuals" [] "trigger" "residual-drift" "runtime" "baien-edge" "proposed" {} "rejection" ""})

(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))
(defn- pyround6 [x] (/ (Math/round (* (double x) 1000000.0)) 1000000.0))

(defn propose-update [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "model_id" (get state "model_id" (get cs "model_id"))
                  "from_version" (long (get state "from_version" (get cs "from_version")))
                  "alpha" (double (get state "alpha" (get cs "alpha")))
                  "prior_bias" (double (get state "prior_bias" (get cs "prior_bias")))
                  "prior_var_infl" (double (get state "prior_var_infl" (get cs "prior_var_infl")))
                  "residuals" (vec (get state "residuals" (get cs "residuals")))
                  "trigger" (norm (get state "trigger" (get cs "trigger")))
                  "runtime" (norm (get state "runtime" (get cs "runtime"))))]
    (cond
      (not= (get cs "runtime") "baien-edge")
      {"cell_state" (assoc cs "rejection" (str "G8: update runtime " (pr-str (get cs "runtime"))
                                               " must be :baien-edge (Murakumo-only, no commercial GPU)")
                           "phase" "rejected")}
      :else
      (let [errs (mapv #(double (get % "error")) (filter #(contains? % "error") (get cs "residuals")))
            sds (mapv #(double (get % "sd")) (filter #(> (or (get % "sd") 0) 0) (get cs "residuals")))]
        (if (empty? errs)
          {"cell_state" (assoc cs "rejection" "no residuals to learn from; nothing to correct" "phase" "rejected")}
          (let [n (count errs)
                mean-err (/ (reduce + errs) n)
                new-bias (+ (* (- 1.0 (get cs "alpha")) (get cs "prior_bias")) (* (get cs "alpha") mean-err))
                resid-std (if (>= n 2)
                            (let [rv (/ (reduce + (map #(* (- % mean-err) (- % mean-err)) errs)) (- n 1))]
                              (if (> rv 0) (Math/sqrt rv) 0.0))
                            (Math/abs (double (first errs))))
                mean-sd (if (seq sds) (/ (reduce + sds) (count sds)) 1.0)
                raw-infl (if (> mean-sd 0) (/ resid-std mean-sd) 1.0)
                new-var-infl (max 0.25 (min 4.0 (+ (* (- 1.0 (get cs "alpha")) (get cs "prior_var_infl"))
                                                   (* (get cs "alpha") raw-infl))))]
            {"cell_state" (assoc cs "phase" "proposed" "rejection" ""
                                 "proposed" {"modelId" (get cs "model_id") "fromVersion" (get cs "from_version")
                                             "toVersion" (+ (get cs "from_version") 1) "trigger" (get cs "trigger")
                                             "runtime" (get cs "runtime") "biasCorr" (pyround6 new-bias)
                                             "varInfl" (pyround6 new-var-infl) "meanError" (pyround6 mean-err)
                                             "n" n "promoted" false})}))))))

(defn apply-correction
  "Corrected forecast under a proposed update: shift mean by learned bias, scale spread by inflation.
  Returns [corrected-mean corrected-sd]. 1:1 with apply_correction."
  [mean sd bias-corr var-infl]
  [(+ mean bias-corr) (max (* sd var-infl) 1e-9)])

(defn solve [_input-state]
  (throw (ex-info "mitooshi R0 scaffold: activate online_update via Council ADR (post-2606051800 ratification)" {:scaffold true})))
