(ns mitooshi.test-continual-learning
  "Multi-round CONTINUAL-LEARNING / drift-tracking test for mitooshi 見通し. 1:1 port of
  test_continual_learning.py: the EWMA bias correction converges to the true bias over many rounds,
  rejects per-round noise, and RE-converges on a regime drift (keeps tracking, never stuck)."
  (:require [clojure.test :refer [deftest is]]
            [mitooshi.methods.score :as score]
            [mitooshi.cells.online-update.state-machine :as ou]))

(def noise [-0.2 0.1 -0.1 0.2 0.0])
(def raw-mu 10.0)
(def sd 1.0)
(def alpha 0.4)

(defn- one-round [true-bias prior-bias prior-var-infl]
  (let [errs (mapv (fn [d] {"error" (+ true-bias d) "sd" sd}) noise)
        out (ou/propose-update {"cell_state" {} "model_id" "m-cont" "from_version" 1
                                "residuals" errs "runtime" "baien-edge" "alpha" alpha
                                "prior_bias" prior-bias "prior_var_infl" prior-var-infl})
        p (get-in out ["cell_state" "proposed"])]
    (assert (= "proposed" (get-in out ["cell_state" "phase"])))
    [(get p "biasCorr") (get p "varInfl")]))

(defn- run-schedule [schedule]
  (loop [sch schedule c 0.0 vi 1.0 traj []]
    (if (empty? sch)
      traj
      (let [[true-bias n] (first sch)
            [c' vi' traj'] (loop [k n c c vi vi tr traj]
                             (if (zero? k) [c vi tr]
                                 (let [[nc nvi] (one-round true-bias c vi)]
                                   (recur (dec k) nc nvi
                                          (conj tr {:true-bias true-bias :c nc :var-infl nvi
                                                    :deployed-err (Math/abs (- true-bias nc))
                                                    :crps (score/gaussian-crps (+ raw-mu nc) sd (+ raw-mu true-bias))})))))]
        (recur (rest sch) c' vi' traj')))))

(deftest test-converges-to-true-bias
  (let [traj (run-schedule [[2.0 12]])]
    (is (< (Math/abs (- (:c (last traj)) 2.0)) 0.1))))

(deftest test-error-decreases-monotonically-during-convergence
  (let [traj (run-schedule [[2.0 12]])
        errs (mapv :deployed-err traj)]
    (is (every? (fn [i] (<= (nth errs (inc i)) (+ (nth errs i) 1e-9))) (range (dec (count errs)))))
    (is (< (:crps (last traj)) (* 0.5 (:crps (first traj)))))))

(deftest test-tracks-a-regime-drift
  (let [traj (run-schedule [[2.0 12] [4.0 12]])
        mid (nth traj 11) end (last traj)]
    (is (< (Math/abs (- (:c mid) 2.0)) 0.1))
    (is (< (Math/abs (- (:c end) 4.0)) 0.2))))

(deftest test-variance-inflation-stays-bounded
  (is (every? #(<= 0.25 (:var-infl %) 4.0) (run-schedule [[2.0 8] [4.0 8]]))))

(deftest test-noise-is-rejected-not-amplified
  (let [traj (run-schedule [[2.0 15]])
        worst-single (apply max (map #(Math/abs (- (+ 2.0 %) 2.0)) noise))]
    (is (< (Math/abs (- (:c (last traj)) 2.0)) worst-single))))
