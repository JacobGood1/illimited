(ns integration-test
  (:refer-clojure :exclude [sync])
  (:require [clojure.test :refer [deftest is testing]]
            [coroutine :refer [co-gen return finished? race sync wait]]
            [let-mut :refer [let-mut]]))

(deftest race-let-mut-wait-combined
  (testing "race with let-mut counters — wait delays one, the other wins"
    ;; Two coroutines race: a fast counter (no wait) vs a slow counter
    ;; (wait delays its start by one pump). The fast one finishes first.
    (let [counter (fn [step limit]
                    (co-gen
                      (fn []
                        (let-mut [total 0]
                          (loop []
                            (set! total (+ total step))
                            (if (>= total limit)
                              total
                              (do (return total) (recur))))))))
          slow-counter (fn [step limit]
                         (co-gen
                           (fn []
                             (let-mut [total 0]
                               (wait)
                               (loop []
                                 (set! total (+ total step))
                                 (if (>= total limit)
                                   total
                                   (do (return total) (recur))))))))
          r (race (counter 10 30) (slow-counter 5 30))]

      ;; Pump 1: fast yields 10, slow is waiting (delayed start)
      (is (= [10 :coroutine/waiting] (r)))
      (is (not (finished? r)))

      ;; Pump 2: fast yields 20, slow just started → yields 5
      (is (= [20 5] (r)))
      (is (not (finished? r)))

      ;; Pump 3: fast hits 30 and finishes — race ends, slow never pumped
      (is (= [30] (r)))
      (is (finished? r)))))

(deftest sync-with-let-mut-accumulator-and-wait
  (testing "coroutines sharing a pattern: accumulate, wait, then finish"
    (let [;; accumulates values, waits between phases, returns final sum
          phased (fn [phase1-vals phase2-vals]
                   (co-gen
                     (fn []
                       (let-mut [sum 0]
                         ;; phase 1: accumulate and yield each step
                         (doseq [v phase1-vals]
                           (set! sum (+ sum v))
                           (return sum))
                         ;; pause between phases
                         (wait)
                         ;; phase 2: accumulate more
                         (doseq [v phase2-vals]
                           (set! sum (+ sum v))
                           (return sum))
                         sum))))
          s (sync (phased [1 2 3] [10 20])
                  (phased [100]   [200 300 400]))]

      ;; Both start phase 1
      (is (= [1 100] (s)))                    ; co1 yields 1, co2 yields 100
      (is (= [3 :coroutine/waiting] (s)))     ; co1 yields 3, co2 hits wait
      (is (= [6 300] (s)))                    ; co1 yields 6, co2 resumes → yields 300
      (is (= [:coroutine/waiting 600] (s)))   ; co1 hits wait, co2 yields 600
      (is (= [16 1000] (s)))                  ; co1 resumes → yields 16, co2 finishes with 1000
      (is (not (finished? s)))
      (is (= [36 1000] (s)))                  ; co1 finishes with 36, co2 holds 1000
      (is (finished? s)))))
