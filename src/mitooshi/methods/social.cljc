(ns mitooshi.methods.social
  "mitooshi 見通し — aggregate-first resilience advisory + social post (R1, offline).
  Clojure port of methods/social.py (1:1). ADR-2606051800.

  The non-adjudicating delivery layer: turn a forecast DISTRIBUTION over a public
  series into an aggregate-first resilience advisory + an AT Proto social post —
  routed to a PLANNER, never rendered as advice. The charter-clean inverse of a
  'price call'; every gate holds in code:

    G1 distribution-only — a point-asserted forecast is REFUSED; the text always
                           states a band (mean ± sd), never a single number.
    G2 non-speculative   — use must be in the resilience set; trade/speculation refused.
    G3 non-adjudicating  — every advisory MUST name a planner to route to.
    G4 aggregate-first   — posts are anonymized aggregates (shape == 'aggregate').
    no-server-key        — live broadcast is operator-gated; default is a :draft.

  The optional Murakumo narration (kotoba `llm`) is unavailable offline, so narration
  is always nil here — exactly the Python `llm is None` branch. stdlib only."
  (:require [clojure.string :as str]))

;; G2 — the only non-speculative uses; trade/speculation/wager/position are NOT members.
(def ALLOWED-USE [":resilience" ":planning" ":nowcast" ":early-warning" ":research"])
;; G3 — the planners mitooshi may route a resilience advisory to (it never decides itself).
(def PLANNERS ["danjo" "kanae" "watari"])

(defn- pyround
  "round(x, n) with banker's rounding (round-half-to-even), matching Python round().
  Returns a double that str's cleanly for the band magnitudes here (e.g. -0.1, 0.5)."
  [x n]
  (let [f (Math/pow 10.0 n)]
    (/ (Math/rint (* (double x) f)) f)))

(defn- num-str
  "str(float) for the band values — Clojure prints -0.1 / 0.5 / 0.2 as Python does;
  integers-as-doubles keep the trailing .0 (Python str(1.0) == '1.0')."
  [x]
  (str x))

(defn compose-resilience-advisory
  "Compose ONE aggregate-first resilience advisory from a forecast distribution. Refuses
  (ex-info) a point assertion (G1), an illegal use (G2), or a missing/invalid planner
  route (G3). The text states a BAND, never a single value."
  [series mean sd target
   & {:keys [use point-asserted route-to]
      :or {use ":resilience" point-asserted false route-to "danjo"}}]
  (when point-asserted
    (throw (ex-info "G1: mitooshi cannot post a point-asserted forecast (distribution-only)" {})))
  (when-not (some #(= % use) ALLOWED-USE)
    (throw (ex-info (str "G2: use " (pr-str use) " not in the non-speculative set "
                         (pr-str ALLOWED-USE)) {})))
  (when-not (some #(= % route-to) PLANNERS)
    (throw (ex-info (str "G3: a resilience advisory must route to a planner " (pr-str PLANNERS)
                         ", got " (pr-str route-to)) {})))
  (let [lo (pyround (- mean sd) 4)
        hi (pyround (+ mean sd) 4)
        center (pyround mean 4)
        text (str "見通し(分布): 系列 " series " の t=" target " 期待値は概ね ["
                  (num-str lo) ", " (num-str hi) "] の範囲"
                  "(中心 " (num-str center) ")。これは確率分布であり断定的な予測ではありません。"
                  "レジリエンス対応は " route-to " が判断します。")]
    {"series"        series
     "text"          text
     "shape"         "aggregate"            ; G4
     "use"           use                    ; G2
     "pointAsserted" false                  ; G1
     "band68"        [lo hi]
     "routeTo"       route-to               ; G3 — planner decides, mitooshi only states
     "lexicon"       "app.bsky.feed.post"
     "narration"     nil}))                 ; offline: kotoba llm unavailable (Python `llm is None`)

(defn handle-social-post
  "Compose aggregate resilience advisories from forecast records and (optionally) post.
  Each forecast = {series, mean, sd, target, [use], [pointAsserted], [routeTo]}. A point
  assertion (G1), illegal use (G2), or missing planner route (G3) is refused per-item with
  a reason. Live broadcast is operator-gated (no-server-key): without `operatorRef` posts
  are :draft. Aggregate-share is 100% (this layer never targets an individual, G4)."
  [state]
  (let [operator-ref (get state "operatorRef")]
    (loop [fs (get state "forecasts" []), posts [], refused []]
      (if (empty? fs)
        (merge state {"posts" posts
                      "refused" refused
                      "broadcast" (boolean operator-ref)
                      "aggregateSharePct" (if (seq posts) 100 0)})
        (let [f (first fs)
              adv (try
                    (compose-resilience-advisory
                     (get f "series" "?") (double (get f "mean" 0.0))
                     (double (get f "sd" 1.0)) (long (get f "target" 0))
                     :use (get f "use" ":resilience")
                     :point-asserted (boolean (get f "pointAsserted" false))
                     :route-to (get f "routeTo" "danjo"))
                    (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
                      {::refused {"series" (get f "series") "reason" (ex-message e)}}))]
          (if (::refused adv)
            (recur (rest fs) posts (conj refused (::refused adv)))
            (recur (rest fs)
                   (conj posts (assoc adv "state" (if operator-ref "posted" "draft")))
                   refused)))))))
