(ns mitooshi.cells.online-update.test-state-machine
  "Tests for the mitooshi 見通し online_update cell (G8 weight-correction). 1:1 port of the
  online_update portion of cells/test_state_machines.py (ADR-2606051800): propose-update learns the
  systematic bias + variance inflation (PROPOSED, never promoted), apply-correction cancels the
  systematic error, and a non-:baien-edge runtime / empty residuals are REJECTED."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [mitooshi.cells.online-update.state-machine :as ou]))

(deftest test-online-update-proposes-bias-and-inflation
  (let [res [{"error" 2.1 "sd" 1.0} {"error" 1.9 "sd" 1.0} {"error" 2.0 "sd" 1.0}
             {"error" 2.3 "sd" 1.0} {"error" 1.7 "sd" 1.0}]
        cs (get (ou/propose-update {"cell_state" {} "model_id" "m1" "from_version" 1
                                    "residuals" res "trigger" ":residual-drift" "runtime" ":baien-edge"}) "cell_state")]
    (is (= "proposed" (get cs "phase")))
    (is (= 2 (get-in cs ["proposed" "toVersion"])))
    (is (> (get-in cs ["proposed" "biasCorr"]) 0))      ; learned the positive systematic bias
    (is (= false (get-in cs ["proposed" "promoted"])))))  ; promotion is the gate's job

(deftest test-online-update-correction-reduces-systematic-error
  (let [res (vec (repeat 6 {"error" 2.0 "sd" 1.0}))
        bias (get-in (ou/propose-update {"cell_state" {} "model_id" "m" "from_version" 1
                                         "residuals" res "runtime" "baien-edge" "alpha" 1.0})
                     ["cell_state" "proposed" "biasCorr"])
        [new-mu _] (ou/apply-correction 8.0 1.0 bias 1.0)]
    (is (< (Math/abs (- new-mu 10.0)) 1e-6))))

(deftest test-online-update-rejects-commercial-gpu-runtime
  (let [cs (get (ou/propose-update {"cell_state" {} "model_id" "m" "residuals" [{"error" 1.0 "sd" 1.0}]
                                    "runtime" "runpod-a100"}) "cell_state")]
    (is (= "rejected" (get cs "phase")))
    (is (str/includes? (get cs "rejection") "G8"))))

(deftest test-online-update-rejects-empty-residuals
  (is (= "rejected" (get-in (ou/propose-update {"cell_state" {} "model_id" "m" "residuals" [] "runtime" "baien-edge"})
                            ["cell_state" "phase"]))))

(deftest test-online-update-solve-raises
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R0 scaffold" (ou/solve {}))))
