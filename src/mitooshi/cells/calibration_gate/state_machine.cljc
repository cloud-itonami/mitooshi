(ns mitooshi.cells.calibration-gate.state-machine
  "Phase state machine for the mitooshi 見通し calibration_gate cell — the G7/G9/G12 promotion membrane.
  1:1 port of cells/calibration_gate/state_machine.py (ADR-2606051800). A model version is CLEARED for
  promotion only if G12 skill>0 (beats baseline), G7 calibration deviation ≤ max, G9 member/operator
  signature (server sig refused, no-server-key), G1 no point-assertion slipped through. REFUSAL gate."
  (:require [clojure.string :as str]))

(def default-deviation-max 0.4)

(def state-defaults
  {"phase" "init" "model_id" "" "to_version" 2 "skill" 0.0 "deviation" 0.0
   "deviation_max" default-deviation-max "signed_by" "" "point_asserted_any" false "refusal" "" "payload" {}})
(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- round6 [x] (/ (Math/round (* (double x) 1000000.0)) 1000000.0))

(defn review-promotion [state]
  (let [cs (cell-state state)
        cs (assoc cs
                  "model_id" (get state "model_id" (get cs "model_id"))
                  "to_version" (long (get state "to_version" (get cs "to_version")))
                  "skill" (double (get state "skill" (get cs "skill")))
                  "deviation" (double (get state "deviation" (get cs "deviation")))
                  "deviation_max" (double (get state "deviation_max" (get cs "deviation_max")))
                  "signed_by" (or (get state "signed_by" (get cs "signed_by")) "")
                  "point_asserted_any" (boolean (get state "point_asserted_any" (get cs "point_asserted_any"))))
        refuse (fn [msg] {"cell_state" (assoc cs "refusal" msg "phase" "refused")})
        sb (get cs "signed_by")]
    (cond
      (get cs "point_asserted_any")
      (refuse "G1: an evaluated forecast asserts a deterministic point; promotion refused (非終末論)")
      (not (> (get cs "skill") 0.0))
      (refuse (str "G12: model " (pr-str (get cs "model_id")) " skill " (format "%.4f" (get cs "skill")) " ≤ 0; does not beat baseline; promotion refused"))
      (> (get cs "deviation") (get cs "deviation_max"))
      (refuse (str "G7: calibration deviation " (format "%.4f" (get cs "deviation")) " > " (format "%.4f" (get cs "deviation_max")) "; miscalibrated; promotion refused"))
      (or (not (seq sb)) (str/starts-with? (str/lower-case sb) "server"))
      (refuse (str "G9: promotion of " (pr-str (get cs "model_id")) " needs a member/operator signature; server signature refused (no-server-key)"))
      :else
      {"cell_state" (assoc cs "phase" "cleared" "refusal" ""
                           "payload" {"modelId" (get cs "model_id") "toVersion" (get cs "to_version")
                                      "skill" (round6 (get cs "skill")) "deviation" (round6 (get cs "deviation"))
                                      "signedBy" sb "serverHeldKey" false "promoted" true})})))

(defn solve [_input-state]
  (throw (ex-info "mitooshi R0 scaffold: activate calibration_gate via Council ADR (post-2606051800 ratification)" {:scaffold true})))
