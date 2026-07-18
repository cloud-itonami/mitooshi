(ns mitooshi.methods.bridge-kakaku
  "mitooshi 見通し — kakaku 価格 price / supply-demand bridge (R0, offline).
  Clojure port of methods/bridge_kakaku.py (1:1, the pure core). ADR-2606051800 × 2605091200.

  The price analogue of the watari/watatsuna chokepoint bridge (bridge.cljc): lets mitooshi
  FORECAST the very price / supply-demand series kakaku OBSERVES. kakaku derives two public
  series per product:
    :ph/* (price history)  :ph/product + :ph/total-price → kind :price-level         (minor)
    :sd/* (supply/demand)  :sd/product + :sd/index       → kind :supply-demand-index (index)

  Ingested as :representative / source-class :public-broadcast (G4/G11), source actor tagged.
  `bridge-kakaku` is pure (takes already-loaded records); the file read (analyze/load-edn) +
  G10 live wiring live in the Python main, omitted from this port. stdlib only."
  (:require [clojure.string :as str]))

(defn- pslug
  "Slugify a kakaku product id ('jan_4901777300443' / ':jan-…') for a series id."
  [pid]
  (-> (str pid) (str/replace #"^:+" "") (str/replace "_" "-") (str/replace "." "-") str/lower-case))

(defn- mk-series [pid suffix kind unit]
  (let [slug (pslug pid) sid (str "s-" slug "-" suffix)]
    {":series/id" sid ":series/name" (str slug " " kind)
     ":series/kind" (str ":" kind) ":series/unit" unit ":series/freq" ":daily"
     ":series/source" "kakaku price roll-up (DERIVED, public)"
     ":series/source-class" ":public-broadcast" ":series/sourcing" ":representative"}))

(defn- add-obs [acc s pid-val observed-at]
  (let [sid (get s ":series/id")]
    (-> acc
        (assoc-in [:series sid] s)
        (update :obs conj {":obs/id" (str "obs." sid "." observed-at)
                           ":obs/series" sid ":obs/observed-at" observed-at
                           ":obs/value" (double pid-val) ":obs/source-actor" "kakaku"}))))

(defn bridge-kakaku
  "records = kakaku-shaped :ph/* and :sd/* observation maps. Returns {series obs skipped}.
  A :price-level series per product (:ph/total-price) + a :supply-demand-index series per
  product (:sd/index); records lacking a product key are skipped."
  [records observed-at]
  (let [acc (reduce
             (fn [acc rec]
               (cond
                 (and (contains? rec ":ph/product") (contains? rec ":ph/total-price"))
                 (add-obs acc (mk-series (get rec ":ph/product") "price" "price-level" "minor")
                          (get rec ":ph/total-price") observed-at)
                 (and (contains? rec ":sd/product") (contains? rec ":sd/index"))
                 (add-obs acc (mk-series (get rec ":sd/product") "supply-demand" "supply-demand-index" "index")
                          (get rec ":sd/index") observed-at)
                 :else (update acc :skipped inc)))
             {:series {} :obs [] :skipped 0}
             (or records []))]
    {"series" (:series acc) "obs" (:obs acc) "skipped" (:skipped acc)}))

(defn emit-edn
  "Serialise a bridged snapshot to DERIVED :representative kotoba EDN (mirror of _emit_edn)."
  [b observed-at]
  (let [head [(str ";; kakaku-observations.kotoba.edn — bridged from kakaku 価格 @ ts=" observed-at ".")
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
