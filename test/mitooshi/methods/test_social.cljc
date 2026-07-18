(ns mitooshi.methods.test-social
  "Cross-language oracle tests for mitooshi.methods.social — the Clojure port of
  methods/social.py.

  Ported 1:1 from the REAL Python test_social.py (the cross-language oracle):
  the distribution-only band (G1), the non-speculative use guard (G2), the
  planner-route requirement (G3), aggregate-first draft/posted state and the
  per-item refusal of bad forecasts. The band oracle is exact — mean 0.2 / sd 0.3
  → [-0.1, 0.5] (round-to-4 of the float subtraction), pinned both as the numeric
  band68 and as the substring of the rendered text."
  (:require [clojure.test :refer [deftest is testing]]
            [mitooshi.methods.social :as social]))

(deftest advisory-states-a-band-not-a-point
  (let [adv (social/compose-resilience-advisory "s-x" 0.2 0.3 7)]
    (is (= false (get adv "pointAsserted")))          ; G1
    (is (= [-0.1 0.5] (get adv "band68")))
    (is (clojure.string/includes? (get adv "text") "[-0.1, 0.5]"))))

(deftest advisory-refuses-point-assertion-g1
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) #"G1"
                        (social/compose-resilience-advisory "s-x" 0.2 0.3 7 :point-asserted true))))

(deftest advisory-refuses-speculative-use-g2
  (doseq [bad [":trade" ":speculation" ":wager" ":position"]]
    (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) #"G2"
                          (social/compose-resilience-advisory "s-x" 0.2 0.3 7 :use bad)))))

(deftest advisory-requires-planner-route-g3
  (is (thrown-with-msg? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) #"G3"
                        (social/compose-resilience-advisory "s-x" 0.2 0.3 7 :route-to "some-trader")))
  (testing "a valid planner is accepted"
    (let [adv (social/compose-resilience-advisory "s-x" 0.2 0.3 7 :route-to "kanae")]
      (is (= "kanae" (get adv "routeTo")))
      (is (clojure.string/includes? (get adv "text") "kanae")))))

(deftest allowed-use-excludes-trade
  (is (some #(= % ":resilience") social/ALLOWED-USE))
  (is (some #(= % "danjo") social/PLANNERS))
  (doseq [forbidden [":trade" ":speculation" ":wager" ":position"]]
    (is (not (some #(= % forbidden) social/ALLOWED-USE)))))

(deftest social-post-default-is-draft-aggregate
  (let [out (social/handle-social-post {"forecasts" [{"series" "s-x" "mean" 0.2 "sd" 0.3
                                                      "target" 7 "routeTo" "danjo"}]})
        p (first (get out "posts"))]
    (is (= 1 (count (get out "posts"))))
    (is (= "draft" (get p "state")))                  ; operator-gated (no-server-key)
    (is (= "aggregate" (get p "shape")))              ; G4
    (is (= 100 (get out "aggregateSharePct")))))

(deftest social-post-posts-with-operator
  (let [out (social/handle-social-post {"forecasts" [{"series" "s-x" "mean" 0.2 "sd" 0.3
                                                      "target" 7 "routeTo" "danjo"}]
                                        "operatorRef" "op:1"})]
    (is (= "posted" (get (first (get out "posts")) "state")))))

(deftest social-post-refuses-bad-items-per-item
  (let [out (social/handle-social-post
             {"forecasts" [{"series" "ok" "mean" 0.2 "sd" 0.3 "target" 7 "routeTo" "danjo"}
                           {"series" "pt" "mean" 0.2 "sd" 0.3 "target" 7 "pointAsserted" true}
                           {"series" "tr" "mean" 0.2 "sd" 0.3 "target" 7 "use" ":trade"}]})
        reasons (into {} (map (fn [r] [(get r "series") (get r "reason")]) (get out "refused")))]
    (is (= 1 (count (get out "posts"))))              ; only the clean one
    (is (clojure.string/includes? (get reasons "pt") "G1"))
    (is (clojure.string/includes? (get reasons "tr") "G2"))))
