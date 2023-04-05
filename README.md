# stacker

Another implementation of smol. This implementation presents the execution with a stack-based model.

## How to install, update, and uninstall

Please follow these instructions to **install** or **update**

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in the `Do What I Mean` tab of the pop-up window.
4. Set the `Package Source` field to `Stacker` (or `https://github.com/LuKC1024/stacker.git`)
5. Click the `Show Details` button.
6. Set the `Dependencies Mode` field to `Auto`.
7. Click the `Install` button. (If you have already installed, you will see "Update" instead of "Install".) Most buttons will grey out immediately (except the `Abort Install`).
8. After the color of those buttons comes back, you can close the pop-up window. This usually takes less than 1 min.

Alternatively, you if the `raco` tool is availabe, you can install by running `raco pkg install Stacker`.

To **uninstall**

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in the `Currently Installed` tab of the pop-up window.
4. Set the `Filter` field to `stacker`.
5. Select the first result.
6. Click the `Remove` button. A confirmation window will pop up.
7. Click the `Remove` button in the confirmation window. Most buttons will grey out immediately (except the `Abort Install`).
8. After the color of those buttons comes back, you can close the pop-up window. This usually takes less than 1 min.

## How to test whether an installation is successful?

First, make sure you are in the Racket language:

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `Language` | `Choose Language...`. A window will pop up.
3. Select `The Racket Language`.
4. Click the `OK` button.

Run the following program in DrRacket

```racket
#lang stacker/smol/hof

(defvar x 2)
(defvar y 3)
(+ x y)
```

You should see a screenshot like this.

<img width="509" alt="image" src="https://user-images.githubusercontent.com/10260693/188509738-4394c575-9239-404e-a2b8-eac9fdf656f9.png">

## Usage

Usually, you will use the Stacker like other Racket `#lang`s.

```
#lang stacker/smol/hof

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```

If you *only* want to see the (final) result, you can ask the stacker not to show the stack+heap configurations (note the second line).
This way you don't need to click through the configurations and hence can see the result sooner.

```
#lang stacker/smol/hof
#:no-trace

(deffun (fact n)
  (if (zero? n)
      1
      (* (fact (- n 1)) n)))
(fact 3)
```

## Language Levels

1. `fun`
2. `state` adds mutable variables and mutable vectors
3. `hof` adds first-class functions and `let{,rec,*}`

## Grammar

Here is a glossary of `smol` grammar, where `d` stands for definitions, `e` stands for expressions, `c` stands for constants, and `x` and `f` are identifiers (variables).

```
d ::= (defvar x e)
    | (deffun (f x ...) body)
e ::= c
    | x
    | (lambda (x ...) body)
    | (λ (x ...) body)
    | (let ([x e] ...) body)
    | (letrec ([x e] ...) body)
    | (let* ([x e] ...) body)
    | (begin e ... e)
    | (set! x e)
    | (if e e e)
    | (cond [e e] ... [else e])
    | (cond [e e] ...)
    | (e e ...)

body    ::= d ... e ... e
program ::= d ... e ...
```
