# smol-step

Another implementation of [`smol`](https://github.com/shriram/smol). This implementation can show the *stack diagrams* whenever function calls begin and return.

## Installation

1. `DrRacket`
2. `File`
3. `Install Package...`
4. `Package Source` = `https://github.com/LuKC1024/smol-step.git`
5. `Install`

## Usage

If you want to see the stack diagrams

```racket
#lang smol-step/fun

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```

If you don't want to see the stack diagrams (note the second line)

```racket
#lang smol-step/fun
#:no-trace

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```
