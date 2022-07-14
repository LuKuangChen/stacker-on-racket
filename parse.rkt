#lang racket
(provide parse)
(require "io.rkt")
(require syntax/parse)
(require (only-in plait pair some none))

(define-syntax-class constant
  (pattern x:string)
  (pattern x:number)
  (pattern x:boolean)
  (pattern x:char)
  (pattern #(c:literal ...))
  (pattern ((~datum quote) #(c:literal ...)))
  (pattern ((~datum quote) (c:literal ...))))
(define-syntax-class literal
  (pattern x:string)
  (pattern x:number)
  (pattern x:boolean)
  (pattern x:char)
  (pattern #(c:literal ...))
  (pattern #(c:literal ...))
  (pattern (c:literal ...)))
(define-syntax-class d
  (pattern ((~datum defvar) x:identifier e:e))
  (pattern ((~datum deffun) (x1:identifier x2:identifier ...) d1:d ... e1:e ... e2:e)))
(define-syntax-class e
  (pattern x:identifier)
  (pattern ((~datum lambda) (x:identifier ...) d:d ... e1:e ... e2:e))
  (pattern ((~datum λ) (x:identifier ...) d:d ... e1:e ... e2:e))
  (pattern (e1:e e2:e ...))
  (pattern ((~datum let) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e))
  (pattern ((~datum let*) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e))
  (pattern ((~datum letrec) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e))
  (pattern ((~datum begin) e1:e ... e2:e))
  (pattern ((~datum set!) x:identifier e))
  (pattern ((~datum if) e1:e e2:e e3:e))
  (pattern ((~datum cond) [cnd:e when-cond:e] ... [(~datum else) when-else:e]))
  (pattern ((~datum cond) [cnd:e when-cond:e] ...))
  (pattern c:constant))
(define-syntax-class p
  (pattern (d:d ... e:e ...)))

(define (parse prog)
  (syntax-parse prog
    [(d:d ... e:e ...)
     (program (parse-d* #'(d ...))
              (parse-e* #'(e ...)))]))
(define (parse-x* x*) (map parse-x (syntax-e x*)))
(define (parse-x x) (syntax->datum x))
(define (parse-e* expr*)
  (map parse-e (syntax-e expr*)))
(define (parse-x&e* x&e*)
  (map parse-x&e (syntax-e x&e*)))
(define (parse-e&e* e&e*)
  (map parse-e&e (syntax-e e&e*)))
(define (parse-x&e x&e)
  (syntax-parse x&e
    [[x:id e:e]
     (bind (parse-x #'x)
           (parse-e #'e))]))
(define (parse-e&e e&e)
  (syntax-parse e&e
    [[e1:e e2:e]
     (pair (parse-e #'e1)
           (parse-e #'e2))]))
(define (parse-d* d)
  (map parse-d (syntax-e d)))
(define (parse-d def)
  (syntax-parse def
    [((~datum defvar) x:id e:e)
     (d-var (parse-x #'x) (parse-e #'e))]
    [((~datum deffun) (x1:identifier x2:identifier ...) d:d ... e1:e ... e2:e)
     (d-fun (parse-x #'x1)
            (parse-x* #'(x2 ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]))
(define (parse-e expr)
  (syntax-parse expr
    [((~datum lambda) (x:identifier ...) d:d ... e1:e ... e2:e)
     (e-fun (parse-x* #'(x ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]
    [((~datum λ) (x:identifier ...) d:d ... e1:e ... e2:e)
     (e-fun (parse-x* #'(x ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]
    [((~datum let) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e)
     (e-let (parse-x&e* #'([x e1] ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e2 ...))
            (parse-e #'e3))]
    [((~datum let*) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e)
     (e-let* (parse-x&e* #'([x e1] ...))
             (parse-d* #'(d ...))
             (parse-e* #'(e2 ...))
             (parse-e #'e3))]
    [((~datum letrec) ([x:identifier e1:e] ...) d:d ... e2:e ... e3:e)
     (e-letrec (parse-x&e* #'([x e1] ...))
               (parse-d* #'(d ...))
               (parse-e* #'(e2 ...))
               (parse-e #'e3))]
    [((~datum begin) e1:e ... e2:e)
     (e-begin (parse-e* #'(e1 ...))
              (parse-e #'e2))]
    [((~datum set!) x:identifier e1:e)
     (e-set! (parse-x #'x) (parse-e #'e1))]
    [((~datum if) e1:e e2:e e3:e)
     (e-if (parse-e #'e1) (parse-e #'e2) (parse-e #'e3))]
    [((~datum cond) [cnd:e when-cond:e] ... [(~datum else) when-else:e])
     (e-cond (parse-e&e* #'([cnd when-cond] ...))
             (some (parse-e #'when-else)))]
    [((~datum cond) [cnd:e when-cond:e] ...)
     (e-cond (parse-e&e* #'([cnd when-cond] ...))
             (none))]
    [((~datum quote) x:id)
     (e-con (parse-con #''x))]
    [c:constant
     (e-con (parse-con #'c))]
    [(e1:e e2:e ...)
     (e-app (parse-e #'e1)
            (parse-e* #'(e2 ...)))]
    [x:identifier
     (e-var (syntax->datum #'x))]))
(define (parse-con con)
  (syntax-parse con
    [x:number
     (c-num (syntax-e #'x))]
    [x:boolean
     (c-bool (syntax-e #'x))]
    [x:char
     (c-char (syntax-e #'x))]
    [x:string
     (c-str (syntax-e #'x))]
    [((~datum quote) (x:literal ...))
     (c-list (map parse-literal (syntax-e #'(x ...))))]
    [((~datum quote) #(x:literal ...))
     (c-vec (map parse-literal (syntax-e #'(x ...))))]
    [#(x:literal ...)
     (c-vec (map parse-literal (syntax-e #'(x ...))))]))
(define (parse-literal con)
  (syntax-parse con
    [x:number
     (c-num (syntax-e #'x))]
    [x:boolean
     (c-bool (syntax-e #'x))]
    [x:char
     (c-char (syntax-e #'x))]
    [x:string
     (c-str (syntax-e #'x))]
    [#(x:literal ...)
     (c-vec (map parse-literal (syntax-e #'(x ...))))]
    [(x:literal ...)
     (c-list (map parse-literal (syntax-e #'(x ...))))]))
