# cs19-step

Another implementation of HtDP Beginning Student with Abbreviations. This implementation can show the *stack diagrams* whenever function calls begin and return.

## How to install, update, and uninstall

Please follow these instructions to install or update

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in `Do What I Mean` tab of the pop-up window.
4. Set the `Package Source` field to `https://github.com/brownplt/cs19-step.git`
5. Click the `Show Details` butoon.
6. Set the `Dependencies Mode` field to `Auto`.
7. Click the `Install` button. (If you have already installed, you will see "Update" instead of "Install".) Most buttons will grey out immediately (except the `Abort Install`).
8. After the color of those buttons come back, you can close the pop-up window. This usually takes less than 1 min.

To uninstall

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `File` | `Package Manager...`. A window will pop up.
3. Make sure you are in `Currently Installed` tab of the pop-up window.
4. Set the `Filter` field to `cs19-step`.
5. Select the first result.
6. Click the `Remove` button. A confirmation window will pop up.
7. Click the `Remove` button in the confirmation window. Most buttons will grey out immediatel (except the `Abort Install`).
8. After the color of those buttons come back, you can close the pop-up window. This usually takes less than 1 min.

## How to test whether an installation is successful?

First, make sure you are in the Racket language:

1. Make sure you are in the `DrRacket` app.
2. Go to the menu `Language` | `Choose Language...`. A window will pop up.
3. Select `The Racket Language`.
4. Click the `OK` button.

Run the following program in DrRacket

```racket
#lang cs19-step

(define x 2)
(define (add1 n)
  (+ n 1))
(add1 x)
```

You should see a screenshot like this.

<img width="303" alt="image" src="https://user-images.githubusercontent.com/10260693/175200899-1236b6fd-7fee-4f14-9798-78ee05fd8fa8.png">
