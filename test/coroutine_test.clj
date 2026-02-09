(ns coroutine-test
  (:refer-clojure :exclude [sync])
  (:require [clojure.test :refer [deftest is testing]]
            [coroutine :refer [co-gen return return-final finished? race sync wait]]
            [let-mut :refer [let-mut]]))

(deftest generator-returns-fn
  (is (fn? (co-gen (fn [x] x)))))

(deftest simple-return-value
  (let [co-gen (co-gen (fn [x] x))
        co     (co-gen 1)]
    (is (= 1 (co)))
    (is (finished? co))))

(deftest multiple-args
  (let [co-gen (co-gen (fn [a b c] (+ a b c)))
        co     (co-gen 1 2 3)]
    (is (= 6 (co)))
    (is (finished? co))))

(deftest single-yield
  (let [co-gen (co-gen (fn [x] (return x) :done))
        co     (co-gen 42)]
    (is (not (finished? co)))
    (is (= 42 (co)))
    (is (not (finished? co)))
    (is (= :done (co)))
    (is (finished? co))))

(deftest multiple-yields
  (let [co-gen (co-gen (fn [x]
                            (return x)
                            (return (* x 2))
                            (* x 3)))
        co     (co-gen 5)]
    (is (= 5 (co)))
    (is (= 10 (co)))
    (is (= 15 (co)))
    (is (finished? co))))

(deftest yield-different-types
  (let [co-gen (co-gen (fn []
                            (return :keyword)
                            (return "string")
                            (return [1 2 3])
                            {:a 1}))
        co     (co-gen)]
    (is (= :keyword (co)))
    (is (= "string" (co)))
    (is (= [1 2 3] (co)))
    (is (= {:a 1} (co)))
    (is (finished? co))))

(deftest yield-nil
  (let [co-gen (co-gen (fn [] (return nil) :done))
        co     (co-gen)]
    (is (nil? (co)))
    (is (= :done (co)))))

(deftest finished-returns-last-value
  (let [co-gen (co-gen (fn [] (return 1) :final))
        co     (co-gen)]
    (co)
    (co)
    (is (finished? co))
    (is (= :final (co)))
    (is (= :final (co)))))

(deftest send-value-into-coroutine
  (testing "calling with a value becomes the return of yield"
    (let [co-gen (co-gen (fn []
                              (let [v (return :ready)]
                                (return v)
                                :done)))
          co     (co-gen)]
      (is (= :ready (co 99)))    ; returns current :ready, sends 99 → v=99
      (is (= 99 (co)))           ; yields v=99
      (is (not (finished? co)))
      (is (= :done (co)))
      (is (finished? co)))))

(deftest ping-pong
  (let [co-gen (co-gen (fn []
                            (let [a (return 1)
                                  b (return (+ a 10))]
                              (+ b 100))))
        co     (co-gen)]
    (is (= 1 (co 2)))         ; returns 1, sends 2 → a=2
    (is (= 12 (co 3)))        ; returns 12, sends 3 → b=3
    (is (not (finished? co)))
    (is (= 103 (co)))         ; returns final 103
    (is (finished? co))))

(deftest zero-arg-generator
  (let [co-gen (co-gen (fn [] (return :a) (return :b) :c))
        co     (co-gen)]
    (is (= :a (co)))
    (is (= :b (co)))
    (is (= :c (co)))))

(deftest independent-instances
  (testing "two instances from the same generator are independent"
    (let [co-gen (co-gen (fn [x] (return x) (return (* x 2))))
          co1    (co-gen 10)
          co2    (co-gen 20)]
      (is (= 10 (co1)))
      (is (= 20 (co2)))
      (is (= 20 (co1)))
      (is (= 40 (co2))))))

(deftest accumulator
  (let [co-gen (co-gen (fn []
                            (loop [sum 0]
                              (let [v (return sum)]
                                (if (= v :stop)
                                  sum
                                  (recur (+ sum v)))))))
        co     (co-gen)]
    (is (= 0 (co 5)))         ; returns 0, sends 5 → sum=5
    (is (= 5 (co 10)))        ; returns 5, sends 10 → sum=15
    (is (= 15 (co :stop)))    ; returns 15, sends :stop → exits with 15
    (is (finished? co))))

(deftest many-yields
  (let [co-gen (co-gen (fn [n]
                            (dotimes [i n]
                              (return i))
                            :done))
        co     (co-gen 5)]
    (is (= [0 1 2 3 4 :done]
           (vec (repeatedly 6 #(co)))))
    (is (finished? co))))

(deftest race-basic
  (testing "collects yields from all coroutines each round"
    (let [co1 (co-gen (fn [] (return :a) (return :b) :done1))
          co2 (co-gen (fn [] (return :x) (return :y) :done2))
          r   (race co1 co2)]
      (is (= [:a :x] (r)))
      (is (= [:b :y] (r)))
      (is (not (finished? r)))))

  (testing "race finishes when any coroutine finishes"
    (let [co1 (co-gen (fn [] (while true (return 1))))
          co2 (co-gen (fn [] (return 1) :done))
          r   (race co1 co2)]
      (is (= [1 1] (r)))
      (is (not (finished? r)))
      (is (= [1 :done] (r)))
      (is (finished? r)))))

(deftest race-first-finishes-stops-remaining
  (testing "when first coroutine finishes, later ones are not pumped"
    (let [calls (atom 0)
          co1   (co-gen (fn [] (return :a) :done))
          co2   (co-gen (fn [] (swap! calls inc) (return :x)
                                  (swap! calls inc) (return :y)))
          r     (race co1 co2)]
      (is (= [:a :x] (r)))
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      ;; co1 finishes on second pump — co2 should NOT be pumped
      (is (= [:done] (r)))
      (is (finished? r))
      (is (= 2 @calls)))))

(deftest race-last-finishes
  (testing "when the last coroutine finishes, result includes all values"
    (let [co1 (co-gen (fn [] (while true (return :a))))
          co2 (co-gen (fn [] (while true (return :b))))
          co3 (co-gen (fn [] (return :c) :done))
          r   (race co1 co2 co3)]
      (is (= [:a :b :c] (r)))
      (is (not (finished? r)))
      (is (= [:a :b :done] (r)))
      (is (finished? r)))))

(deftest race-middle-finishes
  (testing "when a middle coroutine finishes, later ones are not pumped"
    (let [calls (atom 0)
          co1   (co-gen (fn [] (while true (return :a))))
          co2   (co-gen (fn [] (return :b) :done))
          co3   (co-gen (fn [] (swap! calls inc) (return :c)
                                  (swap! calls inc) (return :d)))
          r     (race co1 co2 co3)]
      (is (= [:a :b :c] (r)))
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      ;; co2 finishes — co3 should NOT be pumped
      (is (= [:a :done] (r)))
      (is (finished? r))
      (is (= 2 @calls)))))

(deftest race-immediate-finish
  (testing "coroutine that returns without yielding finishes on first pump"
    (let [co1 (co-gen (fn [] :immediate))
          co2 (co-gen (fn [] (return :x) :done))
          r   (race co1 co2)]
      (is (= [:immediate] (r)))
      (is (finished? r)))))

(deftest race-finished-returns-cached-result
  (testing "calling a finished race returns the last result repeatedly"
    (let [co1 (co-gen (fn [] (return 1) :done))
          r   (race co1)]
      (r)
      (r)
      (is (finished? r))
      (is (= [:done] (r)))
      (is (= [:done] (r)))
      (is (= [:done] (r))))))

(deftest race-single-coroutine
  (testing "race with a single coroutine"
    (let [co (co-gen (fn [] (return 1) (return 2) :done))
          r  (race co)]
      (is (= [1] (r)))
      (is (= [2] (r)))
      (is (not (finished? r)))
      (is (= [:done] (r)))
      (is (finished? r)))))

(deftest race-many-coroutines
  (testing "race with many participants"
    (let [co1 (co-gen (fn [] (return :a1) (return :a2) :a-done))
          co2 (co-gen (fn [] (return :b1) (return :b2) :b-done))
          co3 (co-gen (fn [] (return :c1) (return :c2) :c-done))
          co4 (co-gen (fn [] (return :d1) (return :d2) :d-done))
          r   (race co1 co2 co3 co4)]
      (is (= [:a1 :b1 :c1 :d1] (r)))
      (is (= [:a2 :b2 :c2 :d2] (r)))
      ;; all finish on the same round — first one to finish wins
      (is (= [:a-done] (r)))
      (is (finished? r)))))

(deftest race-finished?-tracking
  (testing "finished? is false before any coroutine finishes"
    (let [co1 (co-gen (fn [] (return 1) (return 2) :done))
          co2 (co-gen (fn [] (return 3) (return 4) :done))
          r   (race co1 co2)]
      (is (not (finished? r)))
      (r)
      (is (not (finished? r)))
      (r)
      (is (not (finished? r)))
      (r)
      (is (finished? r)))))

(deftest sync-generic-test
  (let [co1 (co-gen (fn [] (return 1)))
        co2 (co-gen (fn [] (return 3) (return 4) :done))
        s   (sync co1 co2)]
    (is (= [1 3] (s)))
    (is (not (finished? s)))
    (is (= [1 4] (s)))
    (is (not (finished? s)))
    (is (= [1 :done] (s)))
    (is (finished? s))))

(deftest sync-single-coroutine
  (testing "sync with one coroutine behaves like calling the coroutine"
    (let [co (co-gen (fn [] (return :a) (return :b) :done))
          s  (sync co)]
      (is (= [:a] (s)))
      (is (= [:b] (s)))
      (is (not (finished? s)))
      (is (= [:done] (s)))
      (is (finished? s)))))

(deftest sync-all-same-length
  (testing "all coroutines finish on the same round"
    (let [co1 (co-gen (fn [] (return 1) :a))
          co2 (co-gen (fn [] (return 2) :b))
          co3 (co-gen (fn [] (return 3) :c))
          s   (sync co1 co2 co3)]
      (is (= [1 2 3] (s)))
      (is (not (finished? s)))
      (is (= [:a :b :c] (s)))
      (is (finished? s)))))

(deftest sync-early-finisher-holds-value
  (testing "coroutine that finishes early holds its last yielded value"
    (let [co1 (co-gen (fn [] (return :x)))
          co2 (co-gen (fn [] (return :a) (return :b) (return :c) :done))
          s   (sync co1 co2)]
      (is (= [:x :a] (s)))
      ;; co1 finishes here (return s nil), value stays :x
      (is (= [:x :b] (s)))
      (is (not (finished? s)))
      (is (= [:x :c] (s)))
      (is (not (finished? s)))
      (is (= [:x :done] (s)))
      (is (finished? s)))))

(deftest sync-multiple-early-finishers
  (testing "multiple coroutines finish at different times"
    (let [co1 (co-gen (fn [] (return 1)))
          co2 (co-gen (fn [] (return 2) (return 22)))
          co3 (co-gen (fn [] (return 3) (return 33) (return 333) :end))
          s   (sync co1 co2 co3)]
      (is (= [1 2 3] (s)))
      ;; co1 finishes
      (is (= [1 22 33] (s)))
      ;; co2 finishes
      (is (= [1 22 333] (s)))
      (is (not (finished? s)))
      ;; co3 finishes
      (is (= [1 22 :end] (s)))
      (is (finished? s)))))

(deftest sync-finished-returns-cached-result
  (testing "calling a finished sync returns the last result"
    (let [co1 (co-gen (fn [] (return 1) :done))
          s   (sync co1)]
      (s)
      (s)
      (is (finished? s))
      (is (= [:done] (s)))
      (is (= [:done] (s))))))

(deftest sync-finished?-tracking
  (testing "finished? transitions correctly"
    (let [co1 (co-gen (fn [] (return 1) :a))
          co2 (co-gen (fn [] (return 2) :b))
          s   (sync co1 co2)]
      (is (not (finished? s)))
      (s)
      (is (not (finished? s)))
      (s)
      (is (finished? s)))))

(deftest sync-immediate-finish
  (testing "coroutine that returns without yielding"
    (let [co1 (co-gen (fn [] :immediate))
          co2 (co-gen (fn [] (return :a) :done))
          s   (sync co1 co2)]
      ;; co1 finishes immediately with :immediate, co2 yields :a
      (is (= [:immediate :a] (s)))
      (is (not (finished? s)))
      (is (= [:immediate :done] (s)))
      (is (finished? s)))))

(deftest sync-many-coroutines
  (testing "sync with many participants"
    (let [co1 (co-gen (fn [] (return :a1) (return :a2) :a))
          co2 (co-gen (fn [] (return :b1) (return :b2) :b))
          co3 (co-gen (fn [] (return :c1) (return :c2) :c))
          co4 (co-gen (fn [] (return :d1) (return :d2) :d))
          s   (sync co1 co2 co3 co4)]
      (is (= [:a1 :b1 :c1 :d1] (s)))
      (is (= [:a2 :b2 :c2 :d2] (s)))
      (is (not (finished? s)))
      (is (= [:a :b :c :d] (s)))
      (is (finished? s)))))

(deftest sync-does-not-pump-finished-coroutines
  (testing "finished coroutines are not pumped again"
    (let [calls (atom 0)
          co1   (co-gen (fn [] (return (swap! calls inc))
                                  (return (swap! calls inc))))
          co2   (co-gen (fn [] (return :x) (return :y) (return :z) :done))
          s     (sync co1 co2)]
      (s)
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      (s)
      (is (= 2 @calls))
      ;; co1 finishes here (return s nil), co2 still going
      (s)
      (is (= 2 @calls))
      ;; co1 should still not be pumped
      (s)
      (is (= 2 @calls)))))

(deftest sync-explicit-return-value-used
  (testing "non-nil return value appears in results on finishing round"
    (let [co1 (co-gen (fn [] (return 1) (return 2) :final))
          s   (sync co1)]
      (is (= [1] (s)))
      (is (= [2] (s)))
      (is (= [:final] (s)))
      (is (finished? s)))))

(deftest thread-safety
  (testing "coroutines on different threads do not interfere"
    (let [results (pmap (fn [x]
                          (let [co-gen (co-gen (fn [n]
                                                    (return n)
                                                    (return (* n 2))
                                                    (* n 3)))
                                co     (co-gen x)]
                            [(co) (co) (co)]))
                        (range 100))]
      (doseq [[x [a b c]] (map vector (range 100) results)]
        (is (= x a))
        (is (= (* x 2) b))
        (is (= (* x 3) c))))))

(deftest return-final-terminates-immediately
  (testing "return-final sets permanent value and marks finished"
    (let [co-gen (co-gen (fn []
                              (return 1)
                              (return-final 42)
                              (return 2)
                              :never-reached))
          co     (co-gen)]
      (is (= 1 (co)))           ; returns 1, advance hits return-final → done
      (is (finished? co))
      (is (= 42 (co)))           ; permanent value
      (is (= 42 (co))))))        ; still 42

(deftest return-final-without-prior-yields
  (testing "return-final as first action"
    (let [co-gen (co-gen (fn [] (return-final :instant)))
          co     (co-gen)]
      (is (= :instant (co)))
      (is (finished? co))
      (is (= :instant (co))))))

(deftest return-final-with-send-value
  (testing "return-final based on received value"
    (let [co-gen (co-gen (fn []
                              (let [v (return :waiting)]
                                (return-final (* v 10)))))
          co     (co-gen)]
      (is (= :waiting (co 5)))    ; sends 5, returns :waiting
      (is (= 50 (co)))            ; return-final set 50
      (is (finished? co))
      (is (= 50 (co))))))

(deftest wait-no-arg-waits-one-pump
  (testing "(wait) with no args returns ::coroutine/waiting for exactly one pump"
    (let [co-gen (co-gen (fn []
                              (return :a)
                              (wait)
                              (return :b)
                              :done))
          co     (co-gen)]
      (is (= :a (co)))                   ; returns :a, advances to (wait)
      (is (= :coroutine/waiting (co)))   ; returns ::waiting, advances past wait
      (is (= :b (co)))                   ; returns :b
      (is (= :done (co)))
      (is (finished? co)))))

(deftest wait-returns-waiting-then-continues
  (testing "pumping during wait returns ::coroutine/waiting, then resumes"
    (let [co-gen (co-gen (fn []
                              (return :before)
                              (wait 0.1)
                              (return :after)
                              :done))
          co     (co-gen)]
      (is (= :before (co)))              ; returns :before, advances to wait
      (is (= :coroutine/waiting (co)))   ; wait just started, deadline not passed
      ;; sleep past the deadline
      (Thread/sleep 150)
      (is (= :coroutine/waiting (co)))   ; clears wait, resumes → yields :after, returns ::waiting from the suspend
      (is (= :after (co)))               ; returns :after
      (is (= :done (co)))
      (is (finished? co)))))

(deftest wait-with-zero-seconds
  (testing "wait with 0 seconds passes through immediately"
    (let [co-gen (co-gen (fn []
                              (return :a)
                              (wait 0)
                              (return :b)
                              :done))
          co     (co-gen)]
      (is (= :a (co)))                   ; returns :a, advances to wait(0)
      (is (= :coroutine/waiting (co)))   ; deadline already passed, clears wait, resumes → yields :b
      (is (= :b (co)))                   ; returns :b
      (is (= :done (co)))
      (is (finished? co)))))

(deftest wait-between-returns
  (testing "wait works correctly between normal returns"
    (let [co-gen (co-gen (fn []
                              (return 1)
                              (return 2)
                              (wait 0.05)
                              (return 3)
                              :end))
          co     (co-gen)]
      (is (= 1 (co)))
      (is (= 2 (co)))                    ; advances to wait
      (is (= :coroutine/waiting (co)))   ; still waiting
      (Thread/sleep 100)
      (is (= :coroutine/waiting (co)))   ; deadline passed, clears wait, resumes → yields 3
      (is (= 3 (co)))
      (is (= :end (co)))
      (is (finished? co)))))
