# stacker

Another implementation of smol. This implementation presents the execution with a stack-based model.

## How to install, update, and uninstall

Please follow these instructions to install or update

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in `Do What I Mean` tab of the pop-up window.
4. Set the `Package Source` field to `https://github.com/LuKC1024/stacker.git`
5. Click the `Show Details` button.
6. Set the `Dependencies Mode` field to `Auto`.
7. Click the `Install` button. (If you have already installed, you will see "Update" instead of "Install".) Most buttons will grey out immediately (except the `Abort Install`).
8. After the color of those buttons comes back, you can close the pop-up window. This usually takes less than 1 min.

To uninstall

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in the `Currently Installed` tab of the pop-up window.
4. Set the `Filter` field to `stacker`.
5. Select the first result.
6. Click the `Remove` button. A confirmation window will pop up.
7. Click the `Remove` button in the confirmation window. Most buttons will grey out immediately (except the `Abort Install`).
8. After the color of those buttons come back, you can close the pop-up window. This usually takes less than 1 min.

## How to test whether an installation is successful?

First, make sure you are in the Racket language:

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `Language` | `Choose Language...`. A window will pop up.
3. Select `The Racket Language`.
4. Click the `OK` button.

Run the following program in DrRacket

```racket
#lang stacker/smol/fun

(defvar x 2)
(defvar y 3)
(+ x y)
```

You should see a screenshot like this.

<img width="303" alt="image" src="https://user-images.githubusercontent.com/10260693/172886242-700273b3-87e6-4682-8e45-c7ec04510405.png">

## Usage

If you want to see the stack diagrams

```
#lang stacker/smol/fun

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```

If you don't want to see the stack diagrams (note the second line)

```
#lang stacker/smol/fun
#:no-trace

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```

## Language Levels

1. `fun`
2. `state`: mutable variables and mutable vectors
3. `hof`: higher-order functions and `let{,rec,*}`
