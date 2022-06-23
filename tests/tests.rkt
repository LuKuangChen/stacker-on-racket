#lang racket
(require "./utilities.rkt")

(test-equivalent
 '(
   ; score-by-length:: List-of-strings -> Number
   ; Returns a number that is sum of the lengths of strings in list-strings
   (define (score-by-length list-strings)
     (cond
       [(empty? list-strings) 0]
       [(cons? list-strings)
        (+ (string-length (first list-strings))
           (score-by-length (rest list-strings)))]))
   (score-by-length '("abc" "apple" "John Doe"))
   ))


(test-equivalent
 '(
   ; is-in? :: Any List-of-any -> Boolean
   ; Returns whether a specified value exists in a given list
   (define (is-in? value list-values)
     (cond
       [(empty? list-values) #false]
       [(cons? list-values)
        (cond
          [(equal? value (first list-values)) #true]
          [else (is-in? value (rest list-values))])]))
   (is-in? 2 '(1 2 3))
   ))

(test-equivalent
 '(
   ; words-to-sentence :: List-of-strings -> String
   (define (words-to-sentence list-strings)
     (cond
       [(empty? list-strings) ""]
       [(cons? list-strings)
        (cond
          [(empty? (rest list-strings))
           (string-append (first list-strings) "")]
          [else (string-append (first list-strings)
                               " "
                               (words-to-sentence (rest list-strings)))])]))
   (words-to-sentence '("orange" "and" "apple"))
   ))



(test-equivalent
 '(
   ; elim-contains-char :: Char List-of-strings -> List-of-strings
   ; Removes elements of list with specified character
   (define (elim-contains-char c alos)
     (cond
       [(empty? alos) '()]
       [(cons? alos)
        (cond
          [(member? c (string->list (first alos)))
           (elim-contains-char c (rest alos))]
          [else
           (cons (first alos) (elim-contains-char c (rest alos)))])]))
   (elim-contains-char "a" '("red" "apple" "orange" "pie"))
   ))
; ------------------------------------------------------------------------


(test-equivalent
 '(
   ; valid-words :: List-of-strings List-of-chars -> List-of-strings
   ; Returns elements of a list that contain only the characters specified
   (define (valid-words alos aloc)
     (cond
       [(empty? alos) '()]
       [(cons? alos)
        (cond
          [(contains-all-chars aloc (first alos))
           (cons (first alos)
                 (valid-words (rest alos) aloc))]
          [else (valid-words (rest alos) aloc)])]))

   ; HELPER FUNCTION
   ; contains-all-chars :: List-of-chars String -> Boolean
   ; Returns whether a string contains ONLY characters in list-chars
   (define (contains-all-chars aloc str)
     (cond
       [(empty? (string->list str)) #true]
       [(cons? (string->list str))
        (cond 
          [(empty? aloc) #false]
          [(cons? aloc)
           (cond
             [(member? (first (string->list str)) aloc)
              (contains-all-chars aloc
                                  (list->string (rest (string->list str))))]
             [else #false])])]))

   (valid-words '("foo" "bar" "hello" "aha") '(#\a #\b #\h #\r))
   ))
; ------------------------------------------------------------------------


(test-equivalent
 '(
   ; unique :: List-of-any -> List-of-any
   ; Eliminates duplicate values from a list after the first occurance
   (define (unique aloa)
     (cond
       [(empty? aloa) '()]
       [(cons? aloa)
        (cons (first aloa)
              (unique (remove-val (first aloa) (rest aloa))))]))

   ; HELPER FUNCTION
   ; remove-val :: Any List-of-any -> List-of-any
   ; Removes all instances of a specified value from a list of values
   (define (remove-val val alov)
     (cond
       [(empty? alov) '()]
       [(cons? alov)
        (cond
          [(equal? val (first alov))
           (remove-val val (rest alov))]
          [(not (equal? val (first alov)))
           (cons (first alov) (remove-val val (rest alov)))])]))

   (unique '(1 1 2 1 0 3 2))
   ))
; ------------------------------------------------------------------------


(test-equivalent
 '(
   ; l33t :: List-of-strings -> List-of-strings
   ; Replaces certain chars in a list of strings with chars that stand for numbers
   (define (l33t alos)
     (cond
       [(empty? alos) '()]
       [(cons? alos) (cons (list->string
                            (l33t-char
                             (string->list (first alos))))
                           (l33t (rest alos)))]))

   ; HELPER FUNCTION
   ; l33t-char :: List-of-chars -> List-of-chars 
   ; Performs the following character substitutions to a list of characters:
   ;          E/e -> 3, A/a -> 4, I/i -> 1, O/o -> 0
   (define (l33t-char aloc)
     (cond
       [(empty? aloc) '()]
       [(cons? aloc)
        (cond
          [(member? (first aloc) (list #\A #\a))
           (cons #\4 (l33t-char (rest aloc)))]
          [(member? (first aloc) (list #\E #\e))
           (cons #\3 (l33t-char (rest aloc)))]
          [(member? (first aloc) (list #\I #\i))
           (cons #\1 (l33t-char (rest aloc)))]
          [(member? (first aloc) (list #\O #\o))
           (cons #\0 (l33t-char (rest aloc)))]
          [else (cons (first aloc)
                      (l33t-char (rest aloc)))])]))

   (l33t '("hello" "cs-19" "students"))
   ))


(test-equivalent
 '(
   ; strip-vowels :: List-of-strings -> List-of-strings
   ; Returns the same list of strings, but with all vowels removed
   (define (strip-vowels alos)
     (cond
       [(empty? alos) '()]
       [(cons? alos) 
        (cons (list->string (strip-vowels-str (first alos)))
              (strip-vowels (rest alos)))]))

   ; HELPER FUNCTION
   ; strip-vowels-str :: String -> List-of-chars
   ; Returns the inputted string as a list of characters with vowels removed
   (define (strip-vowels-str str)
     (cond
       [(empty? (string->list str)) '()]
       [(cons? (string->list str))
        (cond
          [(member? (first (string->list str))
                    (list #\A #\a #\E #\e #\I #\i #\O #\o #\U #\u))
           (strip-vowels-str (list->string (rest (string->list str))))]
          [else (cons (first (string->list str))
                      (strip-vowels-str
                       (list->string (rest (string->list str)))))])]))

   (strip-vowels '("hello" "cs-19" "students"))
   ))


(test-equivalent
 '(
   ; singles :: List-of-numbers -> List-of-numbers
   ; Returns the inputted list of numbers with each run of identical numbers removed 

   (define (singles alon)
     (cond
       [(empty? alon) empty]
       [(cons? alon)
        (cons (first alon) (singles (remove-leading-run (first alon) (rest alon))))]))

   (define (remove-leading-run val alon)
     (cond
       [(empty? alon) empty]
       [(cons? alon)
        (cond
          [(equal? val (first alon)) (remove-leading-run val (rest alon))]
          [else alon])]))

   (singles '(1 1 1 2 2 1 2))))


(test-equivalent
 '(
   ; elim-contains-char :: Char List-of-strings -> List-of-strings
   ; Removes elements of list with specified character
   (define (elim-contains-char c alos)
     (filter
      ; String -> Boolean :: Returns whether string s does not contain char c
      (lambda (s)
        (not (member? c (string->list s))))
      alos))

   (elim-contains-char "a" '("red" "apple" "orange" "pie"))
   ))

(test-equivalent
 '(
   ; valid-words :: List-of-strings List-of-chars -> List-of-strings
   ; Returns elements of a list that contain only the characters specified
   (define (valid-words alos aloc)
     (filter
      ; String -> Boolean :: Returns whether a string contains only valid chars
      (lambda (s) (is-valid? s aloc))
      alos))

   ; HELPER FUNCTION
   ; is-valid? :: String List-of-chars -> Boolean
   ; Returns a boolean whether each character in string s is present in aloc
   (define (is-valid? s aloc)
     (andmap
      ; Char -> Boolean :: Returns whether char c is present in aloc
      (lambda (c) (member? c aloc))
      (string->list s)))

   (valid-words '("foo" "bar" "hello" "aha") '(#\a #\b #\h #\r))
   ))

(test-equivalent
 '(
   ; unique :: List-of-any -> List-of-any
   ; Eliminates duplicate values from a list after the first occurance
   (define (unique aloa)
     (foldl cons
            '()
            (foldl cons-if-member '() aloa)))


   ; HELPER FUNCTION
   ; cons-if-member :: Any List-of-any -> List-of-any
   ; Adds val to aloa if it doesn't already exist in the list 
   (define (cons-if-member val aloa)
     (cond
       [(member? val aloa) aloa]
       [else (cons val aloa)]))

   (unique '(1 1 2 1 0 3 2))))

(test-equivalent
 '(
   ; l33t :: List-of-strings -> List-of-strings
   ; Replaces certain chars in a list of strings with chars that stand for numbers
   (define (l33t alos)
     (map
      ; String -> String :: Replaces a string with the l33t-ed string
      (lambda (s) (list->string
                   (map l33t-char (string->list s))))
      alos))

   ; HELPER FUNCTION
   ; l33t-char :: Char -> Char
   ; Performs the following character substitutions to a given character
   ;          E/e -> 3, A/a -> 4, I/i -> 1, O/o -> 0
   (define (l33t-char c)
     (cond
       [(member? c (list #\A #\a)) #\4]
       [(member? c (list #\E #\e)) #\3]
       [(member? c (list #\I #\i)) #\1]
       [(member? c (list #\O #\o)) #\0]
       [else c]))

   (l33t '("hello" "cs-19" "students"))
   ))

(test-equivalent
 '(
   ; strip-vowels :: List-of-strings -> List-of-strings
   ; Returns the same list of strings, but with all vowels removed
   (define (strip-vowels alos)
     (map
      ; String -> String :: Returns inputted string without vowels
      (lambda (s) (list->string
                   (filter is-not-vowel? (string->list s))))
      alos))

   ; HELPER FUNCTION
   ; is-not-vowel? :: Char -> Boolean
   ; Returns whether or not given char is not a vowel
   (define (is-not-vowel? c)
     (cond
       [(member? c (list #\A #\a #\E #\e #\I #\i #\O #\o #\u #\U))
        #false]
       [else #true]))

   (strip-vowels '("hello" "cs-19" "students"))))

(test-equivalent
 '(
   ; singles :: List-of-numbers -> List-of-numbers
   ; Returns the inputted list of numbers with each run of identical numbers removed 
   (define (singles alon)
     (foldl cons empty
            (foldl (lambda (a b)
                     (cond
                       [(empty? b) (cons a b)]
                       [(cons? b)
                        (if (equal? a (first b))
                            b
                            (cons a b))]))
                   empty
                   alon)))

   (singles '(1 1 1 2 2 1 2))
   ))