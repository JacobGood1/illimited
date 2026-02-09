(ns coroutine-test
  (:refer-clojure :exclude [sync])
  (:require [clojure.test :refer [deftest is testing]]
            [coroutine :refer [coroutine yield finished? race sync]]))

(deftest generator-returns-fn
  (is (fn? (coroutine (fn [x] x)))))

(deftest simple-return-value
  (let [co-gen (coroutine (fn [x] x))
        co     (co-gen 1)]
    (is (= 1 (co)))
    (is (finished? co))))

(deftest multiple-args
  (let [co-gen (coroutine (fn [a b c] (+ a b c)))
        co     (co-gen 1 2 3)]
    (is (= 6 (co)))
    (is (finished? co))))

(deftest single-yield
  (let [co-gen (coroutine (fn [x] (yield x) :done))
        co     (co-gen 42)]
    (is (not (finished? co)))
    (is (= 42 (co)))
    (is (not (finished? co)))
    (is (= :done (co)))
    (is (finished? co))))

(deftest multiple-yields
  (let [co-gen (coroutine (fn [x]
                            (yield x)
                            (yield (* x 2))
                            (* x 3)))
        co     (co-gen 5)]
    (is (= 5 (co)))
    (is (= 10 (co)))
    (is (= 15 (co)))
    (is (finished? co))))

(deftest yield-different-types
  (let [co-gen (coroutine (fn []
                            (yield :keyword)
                            (yield "string")
                            (yield [1 2 3])
                            {:a 1}))
        co     (co-gen)]
    (is (= :keyword (co)))
    (is (= "string" (co)))
    (is (= [1 2 3] (co)))
    (is (= {:a 1} (co)))
    (is (finished? co))))

(deftest yield-nil
  (let [co-gen (coroutine (fn [] (yield nil) :done))
        co     (co-gen)]
    (is (nil? (co)))
    (is (= :done (co)))))

(deftest finished-returns-last-value
  (let [co-gen (coroutine (fn [] (yield 1) :final))
        co     (co-gen)]
    (co)
    (co)
    (is (finished? co))
    (is (= :final (co)))
    (is (= :final (co)))))

(deftest send-value-into-coroutine
  (testing "calling with a value becomes the return of yield"
    (let [co-gen (coroutine (fn []
                              (let [v (yield :ready)]
                                (yield v)
                                :done)))
          co     (co-gen)]
      (is (= :ready (co 99)))    ; returns current :ready, sends 99 → v=99
      (is (= 99 (co)))           ; yields v=99
      (is (not (finished? co)))
      (is (= :done (co)))
      (is (finished? co)))))

(deftest ping-pong
  (let [co-gen (coroutine (fn []
                            (let [a (yield 1)
                                  b (yield (+ a 10))]
                              (+ b 100))))
        co     (co-gen)]
    (is (= 1 (co 2)))         ; returns 1, sends 2 → a=2
    (is (= 12 (co 3)))        ; returns 12, sends 3 → b=3
    (is (not (finished? co)))
    (is (= 103 (co)))         ; returns final 103
    (is (finished? co))))

(deftest zero-arg-generator
  (let [co-gen (coroutine (fn [] (yield :a) (yield :b) :c))
        co     (co-gen)]
    (is (= :a (co)))
    (is (= :b (co)))
    (is (= :c (co)))))

(deftest independent-instances
  (testing "two instances from the same generator are independent"
    (let [co-gen (coroutine (fn [x] (yield x) (yield (* x 2))))
          co1    (co-gen 10)
          co2    (co-gen 20)]
      (is (= 10 (co1)))
      (is (= 20 (co2)))
      (is (= 20 (co1)))
      (is (= 40 (co2))))))

(deftest accumulator
  (let [co-gen (coroutine (fn []
                            (loop [sum 0]
                              (let [v (yield sum)]
                                (if (= v :stop)
                                  sum
                                  (recur (+ sum v)))))))
        co     (co-gen)]
    (is (= 0 (co 5)))         ; returns 0, sends 5 → sum=5
    (is (= 5 (co 10)))        ; returns 5, sends 10 → sum=15
    (is (= 15 (co :stop)))    ; returns 15, sends :stop → exits with 15
    (is (finished? co))))

(deftest many-yields
  (let [co-gen (coroutine (fn [n]
                            (dotimes [i n]
                              (yield i))
                            :done))
        co     (co-gen 5)]
    (is (= [0 1 2 3 4 :done]
           (vec (repeatedly 6 #(co)))))
    (is (finished? co))))

(deftest race-basic
  (testing "collects yields from all coroutines each round"
    (let [co1 (coroutine (fn [] (yield :a) (yield :b) :done1))
          co2 (coroutine (fn [] (yield :x) (yield :y) :done2))
          r   (race co1 co2)]
      (is (= [:a :x] (r)))
      (is (= [:b :y] (r)))
      (is (not (finished? r)))))

  (testing "race finishes when any coroutine finishes"
    (let [co1 (coroutine (fn [] (while true (yield 1))))
          co2 (coroutine (fn [] (yield 1) :done))
          r   (race co1 co2)]
      (is (= [1 1] (r)))
      (is (not (finished? r)))
      (is (= [1 :done] (r)))
      (is (finished? r)))))

(deftest race-first-finishes-stops-remaining
  (testing "when first coroutine finishes, later ones are not pumped"
    (let [calls (atom 0)
          co1   (coroutine (fn [] (yield :a) :done))
          co2   (coroutine (fn [] (swap! calls inc) (yield :x)
                                  (swap! calls inc) (yield :y)))
          r     (race co1 co2)]
      (is (= [:a :x] (r)))
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      ;; co1 finishes on second pump — co2 should NOT be pumped
      (is (= [:done] (r)))
      (is (finished? r))
      (is (= 2 @calls)))))

(deftest race-last-finishes
  (testing "when the last coroutine finishes, result includes all values"
    (let [co1 (coroutine (fn [] (while true (yield :a))))
          co2 (coroutine (fn [] (while true (yield :b))))
          co3 (coroutine (fn [] (yield :c) :done))
          r   (race co1 co2 co3)]
      (is (= [:a :b :c] (r)))
      (is (not (finished? r)))
      (is (= [:a :b :done] (r)))
      (is (finished? r)))))

(deftest race-middle-finishes
  (testing "when a middle coroutine finishes, later ones are not pumped"
    (let [calls (atom 0)
          co1   (coroutine (fn [] (while true (yield :a))))
          co2   (coroutine (fn [] (yield :b) :done))
          co3   (coroutine (fn [] (swap! calls inc) (yield :c)
                                  (swap! calls inc) (yield :d)))
          r     (race co1 co2 co3)]
      (is (= [:a :b :c] (r)))
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      ;; co2 finishes — co3 should NOT be pumped
      (is (= [:a :done] (r)))
      (is (finished? r))
      (is (= 2 @calls)))))

(deftest race-immediate-finish
  (testing "coroutine that returns without yielding finishes on first pump"
    (let [co1 (coroutine (fn [] :immediate))
          co2 (coroutine (fn [] (yield :x) :done))
          r   (race co1 co2)]
      (is (= [:immediate] (r)))
      (is (finished? r)))))

(deftest race-finished-returns-cached-result
  (testing "calling a finished race returns the last result repeatedly"
    (let [co1 (coroutine (fn [] (yield 1) :done))
          r   (race co1)]
      (r)
      (r)
      (is (finished? r))
      (is (= [:done] (r)))
      (is (= [:done] (r)))
      (is (= [:done] (r))))))

(deftest race-single-coroutine
  (testing "race with a single coroutine"
    (let [co (coroutine (fn [] (yield 1) (yield 2) :done))
          r  (race co)]
      (is (= [1] (r)))
      (is (= [2] (r)))
      (is (not (finished? r)))
      (is (= [:done] (r)))
      (is (finished? r)))))

(deftest race-many-coroutines
  (testing "race with many participants"
    (let [co1 (coroutine (fn [] (yield :a1) (yield :a2) :a-done))
          co2 (coroutine (fn [] (yield :b1) (yield :b2) :b-done))
          co3 (coroutine (fn [] (yield :c1) (yield :c2) :c-done))
          co4 (coroutine (fn [] (yield :d1) (yield :d2) :d-done))
          r   (race co1 co2 co3 co4)]
      (is (= [:a1 :b1 :c1 :d1] (r)))
      (is (= [:a2 :b2 :c2 :d2] (r)))
      ;; all finish on the same round — first one to finish wins
      (is (= [:a-done] (r)))
      (is (finished? r)))))

(deftest race-finished?-tracking
  (testing "finished? is false before any coroutine finishes"
    (let [co1 (coroutine (fn [] (yield 1) (yield 2) :done))
          co2 (coroutine (fn [] (yield 3) (yield 4) :done))
          r   (race co1 co2)]
      (is (not (finished? r)))
      (r)
      (is (not (finished? r)))
      (r)
      (is (not (finished? r)))
      (r)
      (is (finished? r)))))

(deftest sync-generic-test
  (let [co1 (coroutine (fn [] (yield 1)))
        co2 (coroutine (fn [] (yield 3) (yield 4) :done))
        s   (sync co1 co2)]
    (is (= [1 3] (s)))
    (is (not (finished? s)))
    (is (= [1 4] (s)))
    (is (not (finished? s)))
    (is (= [1 :done] (s)))
    (is (finished? s))))

(deftest sync-single-coroutine
  (testing "sync with one coroutine behaves like calling the coroutine"
    (let [co (coroutine (fn [] (yield :a) (yield :b) :done))
          s  (sync co)]
      (is (= [:a] (s)))
      (is (= [:b] (s)))
      (is (not (finished? s)))
      (is (= [:done] (s)))
      (is (finished? s)))))

(deftest sync-all-same-length
  (testing "all coroutines finish on the same round"
    (let [co1 (coroutine (fn [] (yield 1) :a))
          co2 (coroutine (fn [] (yield 2) :b))
          co3 (coroutine (fn [] (yield 3) :c))
          s   (sync co1 co2 co3)]
      (is (= [1 2 3] (s)))
      (is (not (finished? s)))
      (is (= [:a :b :c] (s)))
      (is (finished? s)))))

(deftest sync-early-finisher-holds-value
  (testing "coroutine that finishes early holds its last yielded value"
    (let [co1 (coroutine (fn [] (yield :x)))
          co2 (coroutine (fn [] (yield :a) (yield :b) (yield :c) :done))
          s   (sync co1 co2)]
      (is (= [:x :a] (s)))
      ;; co1 finishes here (returns nil), value stays :x
      (is (= [:x :b] (s)))
      (is (not (finished? s)))
      (is (= [:x :c] (s)))
      (is (not (finished? s)))
      (is (= [:x :done] (s)))
      (is (finished? s)))))

(deftest sync-multiple-early-finishers
  (testing "multiple coroutines finish at different times"
    (let [co1 (coroutine (fn [] (yield 1)))
          co2 (coroutine (fn [] (yield 2) (yield 22)))
          co3 (coroutine (fn [] (yield 3) (yield 33) (yield 333) :end))
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
    (let [co1 (coroutine (fn [] (yield 1) :done))
          s   (sync co1)]
      (s)
      (s)
      (is (finished? s))
      (is (= [:done] (s)))
      (is (= [:done] (s))))))

(deftest sync-finished?-tracking
  (testing "finished? transitions correctly"
    (let [co1 (coroutine (fn [] (yield 1) :a))
          co2 (coroutine (fn [] (yield 2) :b))
          s   (sync co1 co2)]
      (is (not (finished? s)))
      (s)
      (is (not (finished? s)))
      (s)
      (is (finished? s)))))

(deftest sync-immediate-finish
  (testing "coroutine that returns without yielding"
    (let [co1 (coroutine (fn [] :immediate))
          co2 (coroutine (fn [] (yield :a) :done))
          s   (sync co1 co2)]
      ;; co1 finishes immediately with :immediate, co2 yields :a
      (is (= [:immediate :a] (s)))
      (is (not (finished? s)))
      (is (= [:immediate :done] (s)))
      (is (finished? s)))))

(deftest sync-many-coroutines
  (testing "sync with many participants"
    (let [co1 (coroutine (fn [] (yield :a1) (yield :a2) :a))
          co2 (coroutine (fn [] (yield :b1) (yield :b2) :b))
          co3 (coroutine (fn [] (yield :c1) (yield :c2) :c))
          co4 (coroutine (fn [] (yield :d1) (yield :d2) :d))
          s   (sync co1 co2 co3 co4)]
      (is (= [:a1 :b1 :c1 :d1] (s)))
      (is (= [:a2 :b2 :c2 :d2] (s)))
      (is (not (finished? s)))
      (is (= [:a :b :c :d] (s)))
      (is (finished? s)))))

(deftest sync-does-not-pump-finished-coroutines
  (testing "finished coroutines are not pumped again"
    (let [calls (atom 0)
          co1   (coroutine (fn [] (yield (swap! calls inc))
                                  (yield (swap! calls inc))))
          co2   (coroutine (fn [] (yield :x) (yield :y) (yield :z) :done))
          s     (sync co1 co2)]
      (s)
      (is (= 2 @calls))      ; auto-start ran first swap!, first pump ran second
      (s)
      (is (= 2 @calls))
      ;; co1 finishes here (returns nil), co2 still going
      (s)
      (is (= 2 @calls))
      ;; co1 should still not be pumped
      (s)
      (is (= 2 @calls)))))

(deftest sync-explicit-return-value-used
  (testing "non-nil return value appears in results on finishing round"
    (let [co1 (coroutine (fn [] (yield 1) (yield 2) :final))
          s   (sync co1)]
      (is (= [1] (s)))
      (is (= [2] (s)))
      (is (= [:final] (s)))
      (is (finished? s)))))

(deftest thread-safety
  (testing "coroutines on different threads do not interfere"
    (let [results (pmap (fn [x]
                          (let [co-gen (coroutine (fn [n]
                                                    (yield n)
                                                    (yield (* n 2))
                                                    (* n 3)))
                                co     (co-gen x)]
                            [(co) (co) (co)]))
                        (range 100))]
      (doseq [[x [a b c]] (map vector (range 100) results)]
        (is (= x a))
        (is (= (* x 2) b))
        (is (= (* x 3) c))))))





