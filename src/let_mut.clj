(ns let-mut
  (:require [clojure.walk :as walk]))

(definterface IMutBox
  (getVal [])
  (setVal [v]))

(deftype MutBox [^:unsynchronized-mutable val]
  IMutBox
  (getVal [_] val)
  (setVal [_ v] (set! val v) v))

(defn- pattern-syms [form]
  (into #{} (comp (filter symbol?) (remove #{'& '_})) (tree-seq coll? seq form)))

(defn- xf [syms form]
  (if (empty? syms)
    form
    (let [r (partial xf syms)]
      (cond
        ;; set! on mutable binding
        (and (seq? form) (= 'set! (first form)) (syms (second form)))
        `(.setVal ~(second form) ~(xf syms (nth form 2)))

        ;; let-like: transform inits, shadow bound syms in body
        (and (seq? form)
             (#{'let 'let* 'loop 'loop* 'let-mut 'if-let 'when-let 'with-open} (first form))
             (vector? (second form)))
        (let [[head bindings & body] form
              [new-bindings final-syms]
              (reduce (fn [[binds cur-syms] [pat init]]
                        [(into binds [pat (xf cur-syms init)])
                         (apply disj cur-syms (pattern-syms pat))])
                      [[] syms] (partition 2 bindings))]
          (apply list head (vec new-bindings)
                 (map (partial xf final-syms) body)))

        ;; fn: shadow arg syms in body
        (and (seq? form) (#{'fn 'fn*} (first form)))
        (let [elems (rest form)
              [fname elems] (if (symbol? (first elems))
                              [(first elems) (rest elems)]
                              [nil elems])
              prefix (into ['fn] (when fname [fname]))
              arities (if (vector? (first elems)) [elems] elems)
              xf-arity (fn [[args & body]]
                          (let [inner (apply disj syms (pattern-syms args))]
                            (apply list args (map (partial xf inner) body))))]
          (apply list (concat prefix (map xf-arity arities))))

        ;; mutable symbol reference
        (and (symbol? form) (syms form))
        `(.getVal ~form)

        ;; generic collection
        (coll? form)
        (walk/walk r identity form)

        :else form))))

(defmacro let-mut [bindings & body]
  (assert (vector? bindings) "let-mut requires a vector for bindings")
  (assert (even? (count bindings)) "let-mut requires an even number of binding forms")
  (let [pairs (partition 2 (destructure bindings))
        all-syms (set (map first pairs))
        [result-bindings]
        (reduce (fn [[binds bound] [sym init]]
                  [(into binds
                         [(vary-meta sym assoc :tag 'let_mut.IMutBox)
                          `(MutBox. ~(xf bound init))])
                   (conj bound sym)])
                [[] #{}]
                pairs)]
    `(let [~@result-bindings]
       ~@(map (partial xf all-syms) body))))