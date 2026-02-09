# illimited

Illimited continuations for Clojure, powered by GraalVM Espresso.

## Prerequisites

- [GraalVM Espresso JDK](https://www.graalvm.org/jdk21/reference-manual/java-on-truffle/) (tested with 25.0.2)
- [Clojure CLI](https://clojure.org/guides/install_clojure) (`clj`)

Espresso provides Java-on-Truffle with delimited continuation support, which is what makes coroutines possible.

## Getting started

### 1. Install GraalVM Espresso

Download and extract the Espresso JDK. The launcher script expects it at `C:\graalvm\espresso-25.0.2` by default. Edit the `$EspressoHome` path in `espresso.ps1` if yours is elsewhere.

### 2. Start the nREPL

The standard `clj` command doesn't work with Espresso due to a hard-coded HotSpot flag incompatibility. Use the launcher script instead:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File espresso.ps1
```

This resolves the classpath via `clj`, then boots the Espresso JVM with continuation support enabled and starts an nREPL on **port 7890**.

## Usage

```clojure
(require '[coroutine :refer [coroutine return return-final finished? race sync wait]]
         '[let-mut :refer [let-mut]])
```

### Coroutines

```clojure
(def co ((coroutine (fn [x] (return x) (return (* x 2)) (* x 3))) 5))
(co)           ;=> 5
(co)           ;=> 10
(co)           ;=> 15
(finished? co) ;=> true
```

### Wait

Pause a coroutine for one pump or for a duration in seconds:

```clojure
(def co ((coroutine (fn [] (return :ready) (wait) (return :go) :done))))
(co) ;=> :ready
(co) ;=> :coroutine/waiting
(co) ;=> :go
(co) ;=> :done
```

### Race

Pump multiple coroutines in parallel — finishes when the first one completes:

```clojure
(def r (race (coroutine (fn [] (return :a) :done-a))
             (coroutine (fn [] (return :x) (return :y) :done-b))))
(r) ;=> [:a :x]
(r) ;=> [:done-a]  — first coroutine finished, race ends
(finished? r) ;=> true
```

### Sync

Pump multiple coroutines in parallel — finishes when all complete:

```clojure
(def s (sync (coroutine (fn [] (return 1) :a))
             (coroutine (fn [] (return 2) (return 3) :b))))
(s) ;=> [1 2]
(s) ;=> [:a 3]   — first finished, holds :a
(s) ;=> [:a :b]  — both done
(finished? s) ;=> true
```

### Mutable bindings

`let-mut` provides mutable locals inside coroutine bodies:

```clojure
(def co ((coroutine (fn []
                      (let-mut [total 0]
                        (dotimes [_ 3]
                          (set! total (+ total 10))
                          (return total))
                        total)))))
(co) ;=> 10
(co) ;=> 20
(co) ;=> 30
(co) ;=> 30
```

For further reference, see the tests:

- `test/coroutine_test.clj` — coroutines, race, sync, wait
- `test/let_mut_test.clj` — mutable bindings
- `test/integration_test.clj` — combining race, sync, let-mut, and wait
