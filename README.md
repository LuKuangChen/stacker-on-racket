# smol-step

Another implementation of [`smol`](https://github.com/shriram/smol). This implementation can show the *stack diagrams* whenever function calls begin and return.

## How to install, update, and uninstall

Please follow these instructions to install

1. `DrRacket`
2. `File`
3. `Install Package...`
4. `Package Source` = `https://github.com/LuKC1024/smol-step.git`
5. `Install`

Update is basically the same. Once you installed `smol-step`,
the `Install` button will become `Update`.

To uninstall

1. `DrRacket`
2. `File`
3. `Install Package...`
4. `Currently Installed`
5. `Filter` = `smol-step`
6. Select the first result
7. `Remove`

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
