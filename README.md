# smol-step

Another implementation of [`smol`](https://github.com/shriram/smol). This implementation can show the *stack diagrams* whenever function calls begin and return.

## How to install, update, and uninstall

Please follow these instructions to install

1. `DrRacket`
2. `File`
3. `Install Package...`
4. `Package Source` = `https://github.com/LuKC1024/smol-step.git`
5. `Show Details`
6. `Dependencies Mode` = `Auto`
7. `Install`

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

## How to test whether an installation is successful?

First, make sure you are in the Racket language:

1. `DrRacket`
2. `Language`
3. `Choose Language...`
4. `The Racket Language`

Run the following program in DrRacket

```racket
#lang smol-step/fun

(defvar x 2)
(defvar y 3)
(+ x y)
```

You should see a screenshot like this. The number `1834` might be different.

<img width="812" alt="image" src="https://user-images.githubusercontent.com/10260693/172886242-700273b3-87e6-4682-8e45-c7ec04510405.png">

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
