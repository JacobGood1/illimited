(ns let-mut-test
  (:require [clojure.test :refer [deftest is testing]]
            [let-mut :refer [let-mut]]))

(deftest basic-mutation
  (is (= 10 (let-mut [x 1] (set! x 10) x)))
  (is (= 1 (let-mut [x 1] x))))

(deftest multiple-bindings
  (is (= [10 20]
         (let-mut [x 1 y 2]
           (set! x (* x 10))
           (set! y (* y 10))
           [x y]))))

(deftest set!-returns-value
  (is (= 10 (let-mut [x 1] (set! x 10)))))

(deftest nested-set!
  (is (= [10 10]
         (let-mut [x 1 y 2]
           (set! x (set! y 10))
           [x y]))))

(deftest vector-destructuring
  (is (= 1 (let-mut [[x] [1]] x)))
  (is (= [10 20]
         (let-mut [[x y] [1 2]]
           (set! x (* x 10))
           (set! y (* y 10))
           [x y]))))

(deftest map-destructuring
  (is (= [99 2]
         (let-mut [{:keys [a b]} {:a 1 :b 2}]
           (set! a 99)
           [a b]))))

(deftest nested-let-mut-shadow
  (testing "inner reads outer value"
    (is (= 1 (let-mut [x 1] (let-mut [x x] x) x))))
  (testing "inner set! doesn't affect outer"
    (is (= 1 (let-mut [x 1] (let-mut [x x] (set! x 99) x) x))))
  (testing "inner sees outer mutation"
    (is (= 99 (let-mut [x 1] (set! x 10) (let-mut [x x] (set! x 99) x)))))
  (testing "outer unchanged after inner set!"
    (is (= 10 (let-mut [x 1] (set! x 10) (let-mut [x x] (set! x 99)) x))))
  (testing "partial shadow, other binding visible"
    (is (= [99 2] (let-mut [x 1 y 2] (let-mut [x 99] [x y]))))))

(deftest let-shadows-mutable
  (is (= 5 (let-mut [x 1] (let [x 5] x)))))

(deftest fn-shadows-mutable
  (is (= 42 (let-mut [x 1] ((fn [x] x) 42))))
  (is (= [50 42]
         (let-mut [x 1]
           (set! x 50)
           [x ((fn [x] x) 42)]))))

(deftest loop-shadows-mutable
  (is (= 10 (let-mut [x 1] (loop [x 10] x)))))

(deftest even-bindings-assertion
  (is (thrown? Throwable
        (eval '(let-mut/let-mut [x])))))

(deftest vector-bindings-assertion
  (is (thrown? Throwable
        (eval '(let-mut/let-mut (x 1) x)))))

