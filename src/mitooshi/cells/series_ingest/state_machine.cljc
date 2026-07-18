(ns mitooshi.cells.series-ingest.state-machine
  "Phase state machine for the mitooshi 見通し series_ingest cell — the G4 source membrane.
  1:1 port of cells/series_ingest/state_machine.py (ADR-2606051800). A public series is RECORDED only
  if :series/source-class ∈ the primary-public set; proprietary terminals / scraped Google-Trends are
  unrepresentable and REFUSE ingest. Observations are append-only (非終末論). REFUSAL gate, not a clamp."
  (:require [clojure.string :as str]))

(def allowed-source-class
  #{"public-broadcast" "primary-disclosure" "open-commons" "gov-open-data" "member-principal"})

(def state-defaults
  {"phase" "init" "series_id" "" "source_class" "" "source" "" "kind" "" "obs" [] "refusal" "" "payload" {}})
(defn- cell-state [state] (merge state-defaults (get state "cell_state" {})))
(defn- norm [v] (str/replace (str (or v "")) #"^:+" ""))

(defn transition-to-screened [state]
  (let [cs (cell-state state)
        cs (assoc cs "series_id" (get state "series_id" (get cs "series_id"))
                  "source_class" (norm (get state "source_class" (get cs "source_class")))
                  "source" (get state "source" (get cs "source"))
                  "kind" (norm (get state "kind" (get cs "kind")))
                  "obs" (vec (get state "obs" (get cs "obs"))))]
    (if-not (contains? allowed-source-class (get cs "source_class"))
      {"cell_state" (assoc cs "refusal" (str "G4: series " (pr-str (get cs "series_id")) " source-class "
                                             (pr-str (get cs "source_class")) " not in the primary-public set "
                                             (pr-str (vec (sort allowed-source-class)))
                                             "; proprietary terminals + scraped Google-Trends are unrepresentable (kanjo §2(c)/(e)).")
                           "phase" "refused")}
      {"cell_state" (assoc cs "refusal" "" "phase" "screened")})))

(defn transition-to-recorded [state]
  (let [cs (cell-state state)]
    (if (not= (get cs "phase") "screened")
      {"cell_state" (assoc cs "refusal" (str "cannot record from phase " (pr-str (get cs "phase")) "; screen first") "phase" "refused")}
      (let [obs (vec (sort-by #(get % "observed_at" 0) (get cs "obs")))]
        {"cell_state" (assoc cs "phase" "recorded"
                             "payload" {"seriesId" (get cs "series_id") "sourceClass" (get cs "source_class")
                                        "kind" (get cs "kind") "obsCount" (count obs)
                                        "latestAt" (when (seq obs) (get (last obs) "observed_at"))
                                        "latestValue" (when (seq obs) (get (last obs) "value")) "recorded" true})}))))

(defn solve [_input-state]
  (throw (ex-info "mitooshi R0 scaffold: activate series_ingest via Council ADR (post-2606051800 ratification)" {:scaffold true})))
