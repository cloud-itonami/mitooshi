(ns mitooshi.test-learning-loop
  "ONE-step learning-loop test for mitooshi 見通し. 1:1 port of test_learning_loop.py: score v1 →
  learn the correction from residuals (online_update) → apply → re-score → gate the promotion. The
  model improves on both CRPS and calibration, self-skill > 0, promotion clears signed / refused unsigned (G9)."
  (:require [clojure.test :refer [deftest is]]
            [mitooshi.methods.score :as score]
            [mitooshi.cells.online-update.state-machine :as ou]
            [mitooshi.cells.calibration-gate.state-machine :as cg]))

(def ys [10.0 12.0 9.0 13.0 11.0])
(def mu-v1 [8.4 9.5 8.0 10.0 9.0])
(def sd-v1 1.0)

(defn- score-batch [mus sd yvals]
  (let [rows (mapv (fn [mu y]
                     (let [sc (score/score-pair (score/->forecast "f" "gaussian" :info-as-of 0 :mean mu :sd sd)
                                                (score/->observation "o" :observed-at 1 :value y))]
                       {:crps (get sc "crps") :pit (get sc "pit") :resid {"error" (- y mu) "sd" sd}}))
                   mus yvals)]
    [(/ (reduce + (map :crps rows)) (count rows)) (mapv :pit rows) (mapv :resid rows)]))

(defn- run-cycle []
  (let [[crps-v1 pit-v1 residuals] (score-batch mu-v1 sd-v1 ys)
        calib-v1 (score/calibration-summary pit-v1)
        upd (ou/propose-update {"cell_state" {} "model_id" "m-loop" "from_version" 1
                                "residuals" residuals "runtime" "baien-edge" "alpha" 1.0})
        p (get-in upd ["cell_state" "proposed"])
        bias-corr (get p "biasCorr") var-infl (get p "varInfl")
        v2 (mapv #(ou/apply-correction % sd-v1 bias-corr var-infl) mu-v1)
        mus-v2 (mapv first v2) sd-v2 (second (first v2))
        [crps-v2 pit-v2 _] (score-batch mus-v2 sd-v2 ys)
        calib-v2 (score/calibration-summary pit-v2)
        skill-self (score/skill-score crps-v2 crps-v1)
        promote (fn [signed] (cg/review-promotion {"cell_state" {} "model_id" "m-loop" "to_version" (get p "toVersion")
                                                   "skill" skill-self "deviation" (get calib-v2 "deviation")
                                                   "deviation_max" (get calib-v1 "deviation") "signed_by" signed}))]
    {:crps-v1 crps-v1 :crps-v2 crps-v2 :skill-self skill-self
     :pit-mean-v1 (get calib-v1 "pit_mean") :pit-mean-v2 (get calib-v2 "pit_mean")
     :bias-corr bias-corr :var-infl var-infl
     :promote-signed (get-in (promote "did:web:etzhayyim.com:member:operator") ["cell_state" "phase"])
     :promote-unsigned (get-in (promote "") ["cell_state" "phase"])}))

(deftest test-residuals-recover-the-systematic-bias
  (is (< (Math/abs (- (:bias-corr (run-cycle)) 2.02)) 0.05)))

(deftest test-error-strictly-decreases-after-correction
  (let [r (run-cycle)]
    (is (< (:crps-v2 r) (:crps-v1 r)))
    (is (< (:crps-v2 r) (* 0.6 (:crps-v1 r))))))

(deftest test-calibration-improves-toward-uniform
  (let [r (run-cycle)]
    (is (< (Math/abs (- (:pit-mean-v2 r) 0.5)) (Math/abs (- (:pit-mean-v1 r) 0.5))))
    (is (< (Math/abs (- (:pit-mean-v2 r) 0.5)) 0.2))))

(deftest test-self-skill-is-positive
  (is (> (:skill-self (run-cycle)) 0)))

(deftest test-promotion-clears-when-signed
  (is (= "cleared" (:promote-signed (run-cycle)))))

(deftest test-promotion-refused-without-member-signature
  (is (= "refused" (:promote-unsigned (run-cycle)))))
