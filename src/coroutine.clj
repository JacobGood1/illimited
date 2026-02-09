(ns coroutine
  (:refer-clojure :exclude [sync])
  (:import (clojure.lang IFn)
           [org.graalvm.continuations Continuation ContinuationEntryPoint SuspendCapability]))

(declare finished? co-gen)

(definterface IPumpable
  (resume [v])
  (getOut [])
  (^boolean isFinished []))

(definterface ICoroutine
  (suspend [v])
  (resume [v])
  (getIn [])
  (setIn [v])
  (getOut [])
  (setOut [v])
  (getCap [])
  (setCap [c])
  (getCont ^Continuation [])
  (setCont [c])
  (^boolean isFinished [])
  (markDone [])
  (getWaitUntil [])
  (setWaitUntil [t]))

(def ^:private ^ThreadLocal -active (ThreadLocal.))

(deftype Coroutine [^:unsynchronized-mutable in-val
                    ^:unsynchronized-mutable out-val
                    ^:unsynchronized-mutable ^SuspendCapability cap
                    ^:unsynchronized-mutable ^Continuation cont
                    ^:unsynchronized-mutable done
                    ^:unsynchronized-mutable wait-until]
  IPumpable
  ICoroutine
  (getIn [_] in-val)
  (setIn [_ v] (set! in-val v))
  (getOut [_] out-val)
  (setOut [_ v] (set! out-val v))
  (getCap [_] cap)
  (setCap [_ c] (set! cap c))
  (getCont [_] cont)
  (setCont [_ c] (set! cont c))
  (isFinished [_] done)
  (markDone [_] (set! done true))
  (getWaitUntil [_] wait-until)
  (setWaitUntil [_ t] (set! wait-until t))
  (suspend [this v]
    (set! out-val v)
    (.suspend cap)
    (if (nil? in-val) v in-val))
  (resume [this v]
    (set! in-val v)
    (.set -active this)
    (.resume cont)
    out-val)
  IFn
  (invoke [this]
    (if done
      out-val
      (if-not wait-until
        (let [current out-val]
          (if (.isResumable cont)
            (do (.resume this nil)
                (when-not (.isResumable cont)
                  (when (= out-val current)
                    (set! done true)))
                current)
            (do (set! done true)
                current)))
        (let [wu (long wait-until)]
          (if (neg? wu)
            (do (set! wait-until (+ (System/nanoTime) (- wu)))
                ::waiting)
            (if (> wu (System/nanoTime))
              ::waiting
              (let [_ (set! wait-until nil)
                    current out-val]
                (if (.isResumable cont)
                  (do (.resume this nil)
                      (when-not (.isResumable cont)
                        (when (= out-val current)
                          (set! done true)))
                      current)
                  (do (set! done true)
                      current)))))))))
  (invoke [this v]
    (if done
      out-val
      (if-not wait-until
        (let [current out-val]
          (if (.isResumable cont)
            (do (.resume this v)
                (when-not (.isResumable cont)
                  (when (= out-val current)
                    (set! done true)))
                current)
            (do (set! done true)
                current)))
        (let [wu (long wait-until)]
          (if (neg? wu)
            (do (set! wait-until (+ (System/nanoTime) (- wu)))
                ::waiting)
            (if (> wu (System/nanoTime))
              ::waiting
              (let [_ (set! wait-until nil)
                    current out-val]
                (if (.isResumable cont)
                  (do (.resume this v)
                      (when-not (.isResumable cont)
                        (when (= out-val current)
                          (set! done true)))
                      current)
                  (do (set! done true)
                      current))))))))))

(defn yield
  "Yields a value from inside a coroutine. Suspends execution and returns
   the value passed to the next call."
  [v]
  (.suspend ^ICoroutine (.get -active) v))

(def return
  "Alias for yield."
  yield)

(defn return-final
  "Terminates the coroutine immediately with a permanent value.
   All future calls to the coroutine will return this value."
  [v]
  (let [^ICoroutine co (.get -active)]
    (.setOut co v)
    (.markDone co)
    (.suspend ^SuspendCapability (.getCap co))))

(deftype Race [instances
               ^:unsynchronized-mutable result
               ^:unsynchronized-mutable done]
  IPumpable
  (isFinished [_] done)
  (getOut [_] result)
  (resume [_ v]
    (let [n (count instances)]
      (loop [i 0, acc (transient [])]
        (if (>= i n)
          (let [r (persistent! acc)]
            (set! result r) r)
          (let [co  (nth instances i)
                val (co)]
            (if (finished? co)
              (let [r (persistent! (conj! acc val))]
                (set! result r)
                (set! done true)
                r)
              (recur (inc i) (conj! acc val))))))))
  IFn
  (invoke [this]
    (if done result (.resume this nil))))

(deftype Sync [instances
               ^objects arr
               ^:unsynchronized-mutable remaining
               ^:unsynchronized-mutable done]
  IPumpable
  (isFinished [_] done)
  (getOut [_] (vec arr))
  (resume [_ v]
    (dotimes [i (alength arr)]
      (let [co (nth instances i)]
        (when-not (finished? co)
          (let [val (co)]
            (if (finished? co)
              (do (set! remaining (dec remaining))
                  (when-not (nil? val)
                    (aset arr i val)))
              (aset arr i val))))))
    (when (zero? remaining)
      (set! done true))
    (vec arr))
  IFn
  (invoke [this]
    (if done (vec arr) (.resume this nil))))

(defn- find-args
  "Walks a form and returns the set of %, %1, %2, ... %& symbols."
  [form]
  (cond
    (and (symbol? form)
         (re-matches #"%(&|\d*)?" (name form))) #{form}
    (coll? form) (apply clojure.set/union #{} (map find-args form))
    :else #{}))

(defn- arg-sym->index [s]
  (let [n (name s)]
    (cond
      (= n "%")  1
      (= n "%&") -1
      :else      (parse-long (subs n 1)))))

(defn- build-params [arg-syms]
  (let [indexed (remove #(= -1 %) (map arg-sym->index arg-syms))
        max-n   (if (seq indexed) (apply max indexed) 0)
        params  (mapv #(symbol (str "__gen" %)) (range 1 (inc max-n)))]
    (if (some #(= '%& %) arg-syms)
      (conj params '& (symbol "__gen&"))
      params)))

(defn- replace-args [form params has-rest?]
  (let [positional   (vec (remove #{'&} params))
        replacements (into {'% (first positional)}
                       (map-indexed
                         (fn [i p]
                           [(symbol (str "%" (inc i))) p])
                         positional))
        replacements (if has-rest?
                       (assoc replacements '%& (symbol "__gen&"))
                       replacements)]
    (clojure.walk/postwalk
      (fn [x]
        (if (and (symbol? x) (contains? replacements x))
          (get replacements x)
          x))
      form)))

; ⬇️ API STARTS HERE ⬇️

(defn co-gen
  "Returns a coroutine generator from a function f. Call the generator with
   arguments to create a coroutine instance. The coroutine auto-starts,
   running to the first yield (or completion). Each call returns the current
   yielded value and advances to the next. N yields = N calls.

   (def my-gen (co-gen (fn [x] (yield x) (yield (* x 2)))))
   (def co (my-gen 5))
   (co)            ;=> 5
   (co)            ;=> 10
   (finished? co)  ;=> true"
  [f]
  (fn [& args]
    (let [^ICoroutine co (Coroutine. nil nil nil nil false nil)
          cont (Continuation/create
                 (fn [cap]
                   (.setCap co cap)
                   (.set -active co)
                   (.setOut co (apply f args))))]
      (.setCont co cont)
      (.set -active co)
      (.resume cont)
      co)))

(defn race
  "Takes coroutine generators and returns a race. Each call pumps all
   coroutines sequentially, collecting results into a vector. The race finishes
   as soon as any coroutine finishes — remaining coroutines are not pumped."
  [& generators]
  (let [instances (mapv #(%) generators)]
    (Race. instances nil false)))

(defn sync
  "Takes coroutine generators and returns a sync. Each call pumps all
   coroutines sequentially, collecting results into a vector. The sync finishes
   only when ALL coroutines have finished. Finished coroutines hold their last
   non-nil value."
  [& generators]
  (let [instances (mapv #(%) generators)]
    (Sync. instances (object-array (count instances)) (count instances) false)))

(defn wait
  "Pauses the coroutine. With no arguments, waits for exactly one pump
   (returns ::waiting once, then resumes). With a seconds argument, each
   pump during the wait period returns ::waiting. Once the deadline passes,
   the next pump resumes normally."
  ([]
   (let [^ICoroutine co (.get -active)]
     (.suspend co ::waiting)
     nil))
  ([seconds]
   (let [^ICoroutine co (.get -active)
         duration (- (long (* seconds 1e9)))]
     (.setWaitUntil co duration)
     (.suspend co ::waiting)
     nil)))

(defn finished?
  "Returns true if the coroutine has completed."
  [co]
  (.isFinished ^IPumpable co))

(defmacro defco
  "Defines a coroutine generator. (defco name [args] body) expands to
   (def name (co-gen (fn [args] body)))"
  [name args & body]
  `(def ~name (co-gen (fn ~args ~@body))))

(defmacro co
  "Coroutine generator shorthand using % args, like #() for anonymous fns.
   (co (yield %))          => (co-gen (fn [__gen1] (yield __gen1)))
   (co (+ (yield %1) %2))  => (co-gen (fn [__gen1 __gen2] (+ (yield __gen1) __gen2)))"
  [& body]
  (let [arg-syms  (find-args body)
        has-rest? (contains? arg-syms '%&)
        params    (build-params arg-syms)
        body      (replace-args body params has-rest?)]
    `(co-gen (fn ~params ~@body))))

(defmacro co-default
  "Creates a coroutine with default values, not a generator.
   (co-default [a 1 b 2] (+ a b))  => ((co-gen (fn [a b] (+ a b))) 1 2)"
  [bindings & body]
  (let [params   (vec (take-nth 2 bindings))
        defaults (vec (take-nth 2 (rest bindings)))]
    `((co-gen (fn ~params ~@body)) ~@defaults)))