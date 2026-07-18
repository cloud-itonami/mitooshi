(ns mitooshi.methods.promote
  "mitooshi 見通し — backtest scorecard → promotion decision (R1, offline).
  Clojure port of `methods/promote.py` (ADR-2606051800).

  The last leg of the loop: feed the rolling-origin backtest scorecard into the
  calibration_gate (review-promotion) and emit a PROMOTION DECISION per method. A model
  version only goes live if it CLEARS the gate:
    G12 — beats its baseline (mean-skill > 0),
    G7  — is calibrated (PIT deviation ≤ ceiling),
    G9  — is member/operator-signed (server signature refused — no-server-key),
    G1  — no evaluated forecast asserts a point.

  This is a REFUSAL gate: it never auto-promotes; it refuses anything that fails a gate, and
  live promotion remains G10-gated. The decision is emitted as :fc.promotion datoms.

  IMPLEMENTATION NOTE — importlib dynamic load: the Python original dynamically loads the
  calibration_gate cell's state_machine.py under a unique module name via importlib (to avoid
  state_machine.py name collisions across cells). Per the port conventions, that dynamic load
  becomes an EXPLICIT in-namespace port: `review-promotion` + `DEFAULT-DEVIATION-MAX` are
  ported faithfully below from cells/calibration_gate/state_machine.py (single source of
  truth), no eval / no path-load. `decide-from-scorecard` also accepts an injected gate fn as
  a trailing opt (`:review-fn`) for testing.

  Python `\":…\"` map keys + `:…` value atoms stay STRINGS (kebab-keyword opts only at the
  Clojure call surface). Portable .cljc — file I/O only at the #?(:clj) edge."
  (:require [clojure.string :as str]))

;; ── calibration_gate: review_promotion (ported from cells/calibration_gate/state_machine.py)

(def DEFAULT-DEVIATION-MAX
  "PIT L1-deviation ceiling for 'calibrated enough'."
  0.4)

(defn- as-double [x default]
  (cond
    (number? x) (double x)
    (string? x) (try (Double/parseDouble x) (catch Exception _ (double default)))
    (nil? x) (double default)
    :else (double default)))

(defn review-promotion
  "The G1/G7/G9/G12 promotion membrane. Takes a state map (string-keyed, mirroring the Python
  dict) and returns {\"cell_state\" {...}}. A REFUSAL gate: it clears ONLY a model that beats
  its baseline (G12), is calibrated (G7), is member/operator-signed (G9, no-server-key), and
  asserts no point (G1). 1:1 with review_promotion."
  [state]
  (let [model-id        (get state "model_id" "")
        to-version      (long (as-double (get state "to_version" 2) 2))
        skill           (as-double (get state "skill" 0.0) 0.0)
        deviation       (as-double (get state "deviation" 0.0) 0.0)
        deviation-max   (as-double (get state "deviation_max" DEFAULT-DEVIATION-MAX) DEFAULT-DEVIATION-MAX)
        signed-by       (or (get state "signed_by" "") "")
        point-asserted? (boolean (get state "point_asserted_any" false))
        base            {"phase" "init" "model_id" model-id "to_version" to-version
                         "skill" skill "deviation" deviation "deviation_max" deviation-max
                         "signed_by" signed-by "point_asserted_any" point-asserted?
                         "refusal" "" "payload" {}}
        refuse          (fn [msg]
                          {"cell_state" (assoc base "refusal" msg "phase" "refused")})]
    (cond
      ;; G1 — no point assertion may have slipped through.
      point-asserted?
      (refuse "G1: an evaluated forecast asserts a deterministic point; promotion refused (非終末論)")
      ;; G12 — must beat the baseline.
      (not (> skill 0.0))
      (refuse (format "G12: model '%s' skill %.4f ≤ 0; does not beat baseline; promotion refused"
                      model-id skill))
      ;; G7 — must be calibrated enough.
      (> deviation deviation-max)
      (refuse (format "G7: calibration deviation %.4f > %.4f; miscalibrated; promotion refused"
                      deviation deviation-max))
      ;; G9 — no-server-key: member/operator signature required.
      (or (str/blank? signed-by) (str/starts-with? (str/lower-case signed-by) "server"))
      (refuse (format "G9: promotion of '%s' needs a member/operator signature; server signature refused (no-server-key)"
                      model-id))
      :else
      {"cell_state"
       (assoc base
              "phase" "cleared"
              "refusal" ""
              "payload" {"modelId" model-id
                         "toVersion" to-version
                         "skill" (-> skill (* 1e6) Math/round (/ 1e6))
                         "deviation" (-> deviation (* 1e6) Math/round (/ 1e6))
                         "signedBy" signed-by
                         "serverHeldKey" false
                         "promoted" true})})))

;; ── EDN reader (subset: [] {} :kw "str" num bool nil) — ported from analyze.py ──
;; Returns maps whose keys + :…-atoms are kept as STRINGS, exactly as the Python loader
;; (so r[\":fc.score/method\"] in the Python is (get r \":fc.score/method\") here).

(defn- tokens [s]
  (let [re #"[\s,]+|;[^\n]*|(\[|\]|\{|\}|\"(?:\\.|[^\"\\])*\"|[^\s,\[\]{}]+)"]
    (->> (re-seq re s)
         (keep (fn [m] (let [t (if (vector? m) (second m) m)] t))))))

(defn- atom* [^String t]
  (cond
    (str/starts-with? t "\"") (-> (subs t 1 (dec (count t)))
                                  (str/replace "\\\"" "\"")
                                  (str/replace "\\\\" "\\"))
    (= t "true") true
    (= t "false") false
    (= t "nil") nil
    (str/starts-with? t ":") t
    :else (or (try (Long/parseLong t) (catch Exception _ nil))
              (try (Double/parseDouble t) (catch Exception _ nil))
              t)))

(declare parse-from)

(defn- parse-coll [its]
  ;; its is an atom holding the remaining token seq; returns [value remaining]
  (let [t (first @its)]
    (vreset! its (rest @its))
    (parse-from t its)))

(defn- parse-from [t its]
  (cond
    (= t "[") (loop [out []]
                (let [t2 (first @its)]
                  (vreset! its (rest @its))
                  (if (= t2 "]")
                    out
                    (recur (conj out (parse-from t2 its))))))
    (= t "{") (loop [out {}]
                (let [t2 (first @its)]
                  (vreset! its (rest @its))
                  (if (= t2 "}")
                    out
                    (let [k (parse-from t2 its)
                          v (parse-coll its)]
                      (recur (assoc out k v))))))
    :else (atom* t)))

(defn parse-edn
  "Parse the first EDN value from a token seq (the analyze.py _parse entry point)."
  [toks]
  (let [its (volatile! toks)]
    (parse-coll its)))

(defn load-edn-str
  "Parse an EDN string into the analyze.py value shape (string-keyed maps; :… atoms stay
  strings). Pure — the host edge supplies the text."
  [s]
  (parse-edn (tokens s)))

#?(:clj
   (defn load-edn
     "Read + parse an EDN file (the analyze.py load_edn). File I/O only at the JVM edge."
     [path]
     (load-edn-str (slurp (str path)))))

;; ── decide ────────────────────────────────────────────────────────────────

(defn decide-from-scorecard
  "Run each scorecard method through the calibration_gate. Returns decision rows
  {:method :skill :deviation :phase :refusal :promoted}.

  Trailing opts: {:signed-by \"\" :deviation-max DEFAULT :review-fn review-promotion}. The
  injected :review-fn is the explicit port of the Python importlib dynamic load — pass a stub
  to redirect the gate in tests; defaults to the in-namespace `review-promotion`."
  ([rows] (decide-from-scorecard rows {}))
  ([rows {:keys [signed-by deviation-max review-fn]
          :or {signed-by "" deviation-max DEFAULT-DEVIATION-MAX review-fn review-promotion}}]
   (into []
         (keep (fn [r]
                 (when (contains? r ":fc.score/method")
                   (let [method    (str/replace-first (str (get r ":fc.score/method")) #"^:" "")
                         skill     (as-double (get r ":fc.score/mean-skill" 0.0) 0.0)
                         deviation (as-double (get r ":fc.score/calibration-deviation" 0.0) 0.0)
                         result    (review-fn {"model_id" (str "chokepoint-" method)
                                               "skill" skill
                                               "deviation" deviation
                                               "deviation_max" deviation-max
                                               "signed_by" signed-by})
                         cs        (get result "cell_state")]
                     {:method method
                      :skill skill
                      :deviation deviation
                      :phase (get cs "phase")
                      :refusal (get cs "refusal")
                      :promoted (boolean (get-in cs ["payload" "promoted"] false))}))))
         rows)))

;; ── emit ──────────────────────────────────────────────────────────────────

(defn- round4 [x]
  (-> (double x) (* 1e4) Math/round (/ 1e4)))

(defn- num->str
  "Render a number the way Python str()/f-string does for these rounded values: integral
  doubles print without a trailing .0 mantissa beyond what Python shows. round(x,4) in Python
  yields a float; e.g. 0.5 → \"0.5\", 0.2 → \"0.2\", 1.3 → \"1.3\"."
  [x]
  (let [d (double x)]
    (if (== d (Math/rint d))
      (str (long d))
      (let [s (str d)]
        ;; strip a trailing .0 that Clojure may add (won't happen here given the == guard)
        s))))

(defn emit-decision-edn
  "calibration_gate decision per method → kotoba EDN string. 1:1 with emit_decision_edn."
  [decisions signed-by]
  (let [header [";; chokepoint-promotion-decision.kotoba.edn — calibration_gate decision per method."
                ";; G12 skill>0 · G7 calibrated · G9 member-signed (no-server-key) · G1 no point."
                ";; A REFUSAL gate: never auto-promotes. Live promotion G10-gated. ADR-2606051800."
                (str ";; signed-by: " (if (str/blank? signed-by) "(unsigned)" signed-by))
                ""
                "["]
        body (for [d decisions]
               (let [refusal (str/replace (:refusal d) "\"" "'")]
                 (str " {:fc.promotion/method :" (:method d)
                      " :fc.promotion/skill " (num->str (round4 (:skill d)))
                      " :fc.promotion/deviation " (num->str (round4 (:deviation d)))
                      " :fc.promotion/phase :" (:phase d)
                      " :fc.promotion/promoted " (if (:promoted d) "true" "false")
                      " :fc.promotion/server-held-key false"
                      " :fc.promotion/refusal \"" refusal "\"}")))]
    (str (str/join "\n" (concat header body ["]"])) "\n")))

;; ── CLI edge (#?(:clj)) ─────────────────────────────────────────────────────

#?(:clj
   (defn- arg-after [argv flag]
     (let [i (.indexOf (vec argv) flag)]
       (when (>= i 0) (nth argv (inc i) nil)))))

#?(:clj
   (defn main
     "CLI: --scorecard PATH [--signed-by DID] [--deviation-max F] [--out OUTDIR]. 1:1 with main."
     [argv]
     (if-not (some #{"--scorecard"} argv)
       (do (println "usage: promote --scorecard PATH [--signed-by DID] [--deviation-max F] [--out OUTDIR]")
           2)
       (let [scorecard     (arg-after argv "--scorecard")
             signed-by     (or (arg-after argv "--signed-by")
                               (System/getenv "MITOOSHI_PROMOTE_SIGNED_BY")
                               "")
             deviation-max (if-let [dm (arg-after argv "--deviation-max")]
                             (Double/parseDouble dm)
                             DEFAULT-DEVIATION-MAX)
             rows          (load-edn scorecard)
             decisions     (decide-from-scorecard rows {:signed-by signed-by
                                                        :deviation-max deviation-max})]
         (when-let [outdir (arg-after argv "--out")]
           (let [dir (clojure.java.io/file outdir)]
             (.mkdirs dir)
             (spit (clojure.java.io/file dir "chokepoint-promotion-decision.kotoba.edn")
                   (emit-decision-edn decisions signed-by))))
         (println (format "mitooshi promotion decision (signed-by: %s, deviation-max %s):"
                          (if (str/blank? signed-by) "(unsigned)" signed-by) deviation-max))
         (doseq [d decisions]
           (let [mark (if (= "cleared" (:phase d)) "CLEARED" "REFUSED")
                 why  (if (= "cleared" (:phase d)) "" (str " — " (:refusal d)))]
             (println (format "  %-12s skill=%s deviation=%s → %s%s"
                              (:method d) (:skill d) (:deviation d) mark why))))
         0))))
