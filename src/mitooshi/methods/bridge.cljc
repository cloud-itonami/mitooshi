(ns mitooshi.methods.bridge
  "mitooshi 見通し — watari / watatsuna chokepoint bridge (R0, offline).
  Clojure port of methods/bridge.py (1:1, the pure core). ADR-2606051800.

  watari 渡り (live moving-craft) and watatsuna 綿津綱 (submarine cables) both emit
  chokepoint-keyed aggregates over the SAME keyword space (:malacca, :luzon-strait,
  :suez-red-sea, :hormuz, …). This bridge maps those aggregates into mitooshi :series +
  :obs datoms, so mitooshi can FORECAST the very chokepoints watari/watatsuna OBSERVE —
  the shared chokepoint keyword is the join (observe → forecast).

    watari    :movement/chokepoint   + :movement/chokepoint-transit → kind :transit-load (vessels)
    watatsuna :resilience/chokepoint + :resilience/chokepoint-load  → kind :cable-load   (Tbps)

  Ingested as PUBLIC :representative observations of the chokepoint (never authoritative),
  source actor tagged (G11), source-class :public-broadcast (G4). Non-chokepoint records
  (lanes, craft, stations) are ignored + counted. `bridge` is pure (takes already-loaded
  records); the file read + the G10 live wiring live in the Python main, omitted from this
  port per the convention. stdlib only."
  (:require [clojure.string :as str]))

;; the shared chokepoint keyword space (watari ∩ watatsuna ∩ mitooshi seed) — documentation
(def KNOWN-CHOKEPOINTS
  [":malacca" ":luzon-strait" ":suez-red-sea" ":hormuz" ":gibraltar"
   ":south-china-sea" ":bab-el-mandeb"])

(defn- slug* [cp] (str/replace (str cp) #"^:+" ""))

(defn- mk-series [cp suffix kind unit actor]
  (let [slug (slug* cp) sid (str "s-" slug "-" suffix)]
    {":series/id" sid ":series/name" (str slug " " kind)
     ":series/kind" (str ":" kind) ":series/unit" unit ":series/freq" ":daily"
     ":series/source" (str actor " chokepoint roll-up (DERIVED, public)")
     ":series/source-class" ":public-broadcast" ":series/sourcing" ":representative"}))

(defn- ingest-actor
  "Fold one actor's records into the {series obs skipped} accumulator."
  [acc records cp-key val-key suffix kind unit actor observed-at]
  (reduce
   (fn [{:keys [series obs skipped]} rec]
     (let [cp (get rec cp-key)]
       (if (or (nil? cp) (= "" cp))                  ; mirror Python `if not cp`
         {:series series :obs obs :skipped (inc skipped)}
         (let [s   (mk-series cp suffix kind unit actor)
               sid (get s ":series/id")]
           {:series (assoc series sid s)
            :obs (conj obs {":obs/id" (str "obs." sid "." observed-at)
                            ":obs/series" sid
                            ":obs/observed-at" observed-at
                            ":obs/value" (double (get rec val-key 0))
                            ":obs/source-actor" actor})
            :skipped skipped}))))
   acc records))

(defn bridge
  "records-by-actor = {\"watari\" [...] \"watatsuna\" [...]}. Returns {series obs skipped}."
  [records-by-actor observed-at]
  (let [a0 {:series {} :obs [] :skipped 0}
        a1 (ingest-actor a0 (get records-by-actor "watari" [])
                         ":movement/chokepoint" ":movement/chokepoint-transit"
                         "transit" "transit-load" "vessels" "watari" observed-at)
        a2 (ingest-actor a1 (get records-by-actor "watatsuna" [])
                         ":resilience/chokepoint" ":resilience/chokepoint-load"
                         "cable" "cable-load" "Tbps" "watatsuna" observed-at)]
    {"series" (:series a2) "obs" (:obs a2) "skipped" (:skipped a2)}))

(defn emit-edn
  "Serialise a bridged snapshot to DERIVED :representative kotoba EDN (mirror of _emit_edn)."
  [b observed-at]
  (let [head [(str ";; chokepoint-observations.kotoba.edn — bridged from watari/watatsuna @ ts=" observed-at ".")
              ";; DERIVED public :representative observations (NOT authoritative). ADR-2606051800." "" "["]
        series-lines (map (fn [s]
                            (str " {:series/id \"" (get s ":series/id") "\" :series/kind "
                                 (get s ":series/kind") " :series/unit \"" (get s ":series/unit")
                                 "\" :series/source-class :public-broadcast :series/sourcing :representative}"))
                          (vals (get b "series")))
        obs-lines (map (fn [o]
                         (str " {:obs/id \"" (get o ":obs/id") "\" :obs/series \"" (get o ":obs/series")
                              "\" :obs/observed-at " (get o ":obs/observed-at") " :obs/value "
                              (get o ":obs/value") " :obs/source-actor \"" (get o ":obs/source-actor") "\"}"))
                       (get b "obs"))]
    (str (str/join "\n" (concat head series-lines obs-lines ["]"])) "\n")))
