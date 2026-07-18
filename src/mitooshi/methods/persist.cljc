(ns mitooshi.methods.persist
  "mitooshi 見通し — append-only chokepoint-intel persistence (R0, offline).
  Clojure port of methods/persist.py (1:1, the bridge-free core). ADR-2606051800.

  PERSISTS successive bridged snapshots into a single durable, APPEND-ONLY kotoba-EDN
  trail — the as-of observation history mitooshi forecasts (非終末論: a later snapshot
  never overwrites an earlier one; the trail only grows).

  Invariants preserved:
    * append-only — append-obs NEVER removes or mutates an existing :obs; a re-run is
      idempotent (dedup by :obs/id), a new :obs/observed-at is additive, no overwrite path.
    * DERIVED / :representative — every record stays :series/sourcing :representative; the
      header says 'DERIVED, do NOT re-ingest as authoritative' (G11/G4).
    * no live ingest — this writes a FILE; live kotoba-server ingest is G10-gated (the
      Python `--live` refusal lives in main, which is omitted from this port).

  The impl depends only on the already-ported same-actor analyze (load-edn-file /
  read-edn). `bridge` is used by main + the test fixtures only, NOT here. stdlib only;
  file I/O at the #?(:clj) edge."
  (:require [clojure.string :as str]
            [mitooshi.methods.analyze :as analyze]
            #?(:clj [clojure.java.io :as io])))

(defn append-obs
  "Append-only merge. Returns [merged added duplicate].
  A duplicate :obs/id is NOT re-added and NOT mutated (idempotent re-run); existing obs
  are never removed (非終末論). Order is stable: existing first, then new in input order."
  [existing incoming]
  (loop [in incoming
         merged (vec existing)
         seen (into #{} (map #(get % ":obs/id") existing))
         added 0 dup 0]
    (if (empty? in)
      [merged added dup]
      (let [o (first in) oid (get o ":obs/id")]
        (if (contains? seen oid)
          (recur (rest in) merged seen added (inc dup))
          (recur (rest in) (conj merged o) (conj seen oid) (inc added) dup))))))

(defn merge-series
  "Union of series definitions keyed by :series/id (first definition wins — a series is
  its identity, its metadata stable across snapshots)."
  [existing incoming]
  (reduce (fn [out [sid s]] (if (contains? out sid) out (assoc out sid s)))
          existing incoming))

(defn emit-trail-edn
  "Serialise the trail to append-only kotoba EDN. Obs sorted by (series, observed-at) for
  readability; still append-only in MEANING (no obs dropped/mutated)."
  [series obs]
  (let [span (when (seq obs) (sort (distinct (map #(get % ":obs/observed-at") obs))))
        head [";; chokepoint-trail.kotoba.edn — APPEND-ONLY as-of intel trail."
              ";; Bridged from watari 渡り (transit-load) + watatsuna 綿津綱 (cable-load)."
              ";; DERIVED public :representative observations — do NOT re-ingest as authoritative."
              ";; 非終末論: snapshots only ACCUMULATE; no obs is ever overwritten. ADR-2606051800."
              (str ";; observed-at span: " (if (seq span) (vec span) "(empty)")
                   "  |  series: " (count series) "  obs: " (count obs))
              ";; Live kotoba-server ingest of this trail is G10-gated (Council Lv6+ + operator)."
              "" "["]
        series-lines
        (map (fn [s]
               (str " {:series/id \"" (get s ":series/id") "\" :series/kind "
                    (get s ":series/kind" ":transit-load") " :series/unit \""
                    (get s ":series/unit" "") "\" :series/source-class :public-broadcast "
                    ":series/sourcing :representative}"))
             (sort-by #(get % ":series/id") (vals series)))
        obs-lines
        (map (fn [o]
               (str " {:obs/id \"" (get o ":obs/id") "\" :obs/series \"" (get o ":obs/series")
                    "\" :obs/observed-at " (get o ":obs/observed-at") " :obs/value "
                    (get o ":obs/value") " :obs/source-actor \""
                    (get o ":obs/source-actor" "?") "\"}"))
             (sort-by (juxt #(get % ":obs/series") #(get % ":obs/observed-at")) obs))]
    (str (str/join "\n" (concat head series-lines obs-lines ["]"])) "\n")))

#?(:clj
   (defn load-trail
     "Read an existing trail file → [{series-id series} [obs...]]. Missing file = empty."
     [path]
     (if-not (.exists (io/file (str path)))
       [{} []]
       (reduce (fn [[series obs] rec]
                 (cond
                   (contains? rec ":series/id") [(assoc series (get rec ":series/id") rec) obs]
                   (contains? rec ":obs/id")     [series (conj obs rec)]
                   :else                          [series obs]))
               [{} []] (analyze/load-edn-file path)))))

#?(:clj
   (defn persist
     "Append a bridged snapshot to the durable trail file (creating it if absent).
     Returns stats {added duplicate total_obs series}."
     [trail-path bridged]
     (let [[ex-series ex-obs]   (load-trail trail-path)
           [merged-obs added dup] (append-obs ex-obs (get bridged "obs"))
           merged-series        (merge-series ex-series (get bridged "series"))]
       (io/make-parents (str trail-path))
       (spit (str trail-path) (emit-trail-edn merged-series merged-obs))
       {"added" added "duplicate" dup
        "total_obs" (count merged-obs) "series" (count merged-series)})))
