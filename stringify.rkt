#lang plait
(require "datatypes.rkt")
(require "utilities.rkt")
(require
  (opaque-type-in racket
                  (Port port?))
  (typed-in racket
            (open-output-string : (-> Port))
            (get-output-string : (Port -> String))
            (write : ('a Port -> Void))
            (append* : ((Listof (Listof 'a)) -> (Listof 'a)))))
(require (opaque-type-in pprint (Doc doc?))
         (typed-in pprint
                   (text : (String -> Doc))
                   (align : (Doc -> Doc))
                   (hang : (Number Doc -> Doc))
                   (v-concat : ((Listof Doc) -> Doc))
                   (vsb-concat : ((Listof Doc) -> Doc))
                   (h-concat : ((Listof Doc) -> Doc))
                   (hs-concat : ((Listof Doc) -> Doc))
                   (pretty-print : (Doc -> String))))

(define (symbol x)
  (text x))

(define (doc-of-any s)
  (let* ([p (open-output-string)]
         [_ (write s p)])
    (text (get-output-string p))))

(define (doc-of-x (x : Symbol))
  (symbol (symbol->string x)))

(define (doc-of-prim p)
  (type-case PrimitiveOp p
    [(po-not)
     (doc-of-x 'not)]
    [(po-left)
     (doc-of-x 'left)]
    [(po-right)
     (doc-of-x 'right)]
    [(po-vlen)
     (doc-of-x 'vlen)]
    [(po-eqp)
     (doc-of-x 'equal?)]
    [(po-equalp)
     (doc-of-x 'equal?)]
    [(po-zerop)
     (doc-of-x 'zero?)]
    [(po-+)
     (doc-of-x '+)]
    [(po--)
     (doc-of-x '-)]
    [(po-*)
     (doc-of-x '*)]
    [(po-/)
     (doc-of-x '/)]
    [(po-<)
     (doc-of-x '<)]
    [(po->)
     (doc-of-x '>)]
    [(po-<=)
     (doc-of-x '<=)]
    [(po->=)
     (doc-of-x '>=)]
    [(po-=)
     (doc-of-x '=)]
    [(po-emptyp)
     (doc-of-x 'empty?)]
    [(po-consp)
     (doc-of-x 'cons?)]
    [(po-pairp)
     (doc-of-x 'pair?)]
    [(po-string-length)
     (doc-of-x 'string-length)]
    [(po-string-append)
     (doc-of-x 'string-append)]
    [(po-string->list)
     (doc-of-x 'string->list)]
    [(po-list->string)
     (doc-of-x 'list->string)]
    [(po-mpair)
     (doc-of-x 'mpair)]
    [(po-set-left!)
     (doc-of-x 'set-left!)]
    [(po-set-right!)
     (doc-of-x 'set-right!)]
    [(po-vref)
     (doc-of-x 'vref)]
    [(po-cons)
     (doc-of-x 'cons)]
    [(po-first)
     (doc-of-x 'first)]
    [(po-rest)
     (doc-of-x 'rest)]
    [(po-vset!)
     (doc-of-x 'vset!)]
    [(po-mvec)
     (doc-of-x 'mvec)]
    [(po-list)
     (doc-of-x 'list)]))

(define (doc-list d*)
  (doc-paren (hs-concat d*)))

(define (doc-paren d)
  (h-concat
   (list
    (text "(")
    (align d)
    (text ")"))))

(define (doc-brack d)
  (h-concat
   (list
    (text "[")
    (align d)
    (text "]"))))

(define (doc-brack-pair d1 d2)
  (doc-brack
   (hs-concat
    (list
     d1
     d2))))

(define (head-body head-element* body)
  (doc-paren
   (v-concat
    (list
     (hs-concat head-element*)
     (hang 1 body)))))

(define (stringify-e the-heap e)
  (local [(define-values (doc-of-e doc-of-addr doc-of-f) ((main #f) the-heap))]
    (pretty-print (doc-of-e e))))
(define (stringify-addr the-heap e)
  (local [(define-values (doc-of-e doc-of-addr doc-of-f) ((main #f) the-heap))]
    (pretty-print (doc-of-addr e))))
(define (stringify-env the-heap env)
  (local [(define-values (doc-of-e doc-of-addr doc-of-f) ((main #f) the-heap))]
    (type-case Env env
    ((none) "_")
    ((some addr) (pretty-print (doc-of-addr addr))))))
(define (stringify-ectx the-heap ectx)
  (local [(define-values (doc-of-e doc-of-addr doc-of-f) ((main #f) the-heap))]
    (ind-List (reverse (map doc-of-f ectx))
              (text "□")
              (lambda (IH x)
                (x IH)))))

(define (main fun-addr?)
  (lambda (the-heap)
    (local ((define (doc-of-addr it)
              (type-case HeapAddress it
                [(ha-user it)
                 (text
                  (let ([printing (format "~a" it)])
                    (type-case HeapValue (heap-ref the-heap (ha-user it))
                      ((h-fun env name arg* def* body)
                       (type-case (Optionof Symbol) name
                         ((some s)
                          (if fun-addr?
                              (string-append printing (format ".~a" s))
                              (format "~a" s)))
                         ((none)
                          printing)))
                      (else
                       printing))))]
                [(ha-prim it)
                 (doc-of-primitive-address it)]))
            (define (doc-of-primitive-address pa)
              (type-case PrimitiveHeapAddress pa
                [(pa-map) (doc-of-x 'map)]
                [(pa-filter) (doc-of-x 'filter)]
                [(pa-memberp) (doc-of-x 'member?)]
                [(pa-foldl) (doc-of-x 'foldl)]
                [(pa-foldr) (doc-of-x 'foldr)]
                [(pa-andmap) (doc-of-x 'andmap)]
                [(pa-ormap) (doc-of-x 'ormap)]
                [(pa-base-env) (doc-of-x 'primordial-env)]
                [(pa-empty) (doc-of-x 'empty)]))
            (define (doc-of-v v)
              (type-case Val v
                ((v-addr it)
                 (symbol (format "@~a" (doc-of-addr it))))
                ((v-prim name)
                 (doc-of-prim name))
                ((v-str it)
                 (doc-of-any it))
                ((v-num it)
                 (doc-of-any it))
                ((v-bool it)
                 (doc-of-any it))
                ((v-char it)
                 (doc-of-any it))
                ((v-empty)
                 (text "'()"))
                ((v-void)
                 (doc-of-x '|#<void>|))))
            (define (doc-of-def def)
              (local ((define-values (x e) def))
                (doc-paren
                 (vsb-concat
                  (list
                   (symbol "defvar")
                   (doc-of-x x)
                   (doc-of-e e))))))
          
            (define (doc-lambda arg* def* body)
              (head-body
               (list
                (symbol "lambda")
                (doc-list arg*))
               (v-concat
                (append
                 def*
                 (list body)))))
            (define (doc-app e*)
              (doc-list e*))
            (define (doc-let bind* body)
              (head-body
               (list
                (symbol "let")
                (doc-paren
                 (v-concat bind*)))
               body))
            (define (doc-letrec bind* body)
              (head-body
               (list
                (symbol "letrec")
                (doc-paren
                 (v-concat bind*)))
               body))
            (define (doc-set! x e)
              (doc-paren
               (vsb-concat
                (list
                 (symbol "set!")
                 x
                 e))))
            (define (doc-begin e*)
              (head-body
               (list
                (symbol "begin"))
               (v-concat e*)))
            (define (doc-if e*)
              (doc-paren
               (hs-concat
                (list
                 (text "if")
                 (v-concat e*)))))
            (define (doc-cond ee*)
              (head-body
               (list
                (symbol "cond"))
               (v-concat
                ee*)))
            (define (doc-of-e e) : Doc
              (type-case Term e
                [(t-quote v)
                 (doc-of-v v)]
                [(t-var x)
                 (doc-of-x x)]
                [(t-fun name arg* def* body)
                 (doc-lambda
                  (map doc-of-x arg*)
                  (map doc-of-def def*)
                  (doc-of-e body))]
                [(t-app fun arg*)
                 (doc-app
                  (map doc-of-e (cons fun arg*)))]
                [(t-let bind* body)
                 (doc-let
                  (map doc-of-xe bind*)
                  (doc-of-e body))]
                [(t-letrec bind* body)
                 (doc-letrec
                  (map doc-of-xe bind*)
                  (doc-of-e body))]
                [(t-set! x e)
                 (doc-set!
                  (doc-of-x x)
                  (doc-of-e e))]
                [(t-begin e* e)
                 (doc-begin (map doc-of-e (append e* (list e))))]
                [(t-if cnd thn els)
                 (doc-if (map doc-of-e (list cnd thn els)))]
                [(t-cond cnd-thn* els)
                 (doc-cond
                  (map doc-of-ee
                       (append
                        cnd-thn*
                        (list (values (t-var 'else) els)))))]))
            (define (doc-of-xe xe)
              (local [(define-values (x e) xe)]
                (doc-of-ee (values (t-var x) e))))
            (define (doc-of-ee [ee : (Term * Term)])
              (local [(define-values (e1 e2) ee)]
                (doc-brack
                 (hs-concat
                  (list
                   (doc-of-e e1)
                   (doc-of-e e2))))))
            (define (doc-of-xv [xv : (Id * Val)])
              (local [(define-values (x v) xv)]
                (doc-brack-pair
                 (doc-of-x x)
                 (doc-of-v v))))
            (define (doc-of-f f)
              (lambda ([□ : Doc])
                (type-case ECFrame f
                  ((F-begin e* e)
                   (head-body
                    (list
                     (symbol "begin"))
                    (v-concat (cons □ (map doc-of-e (append e* (list e)))))))
                  ((F-app v* e*)
                   (doc-list
                    (append
                     (map doc-of-v v*)
                     (cons
                      □
                      (map doc-of-e e*)))))
                  ((F-let xv* x xe* body)
                   (head-body
                    (list
                     (symbol "let")
                     (doc-paren
                      (v-concat
                       (append*
                        (list
                         (map doc-of-xv xv*)
                         (list (doc-brack-pair (doc-of-x x) □))
                         (map doc-of-xe xe*))))))
                    (doc-of-e body)))
                  ((F-if thn els)
                   (doc-if (list □ (doc-of-e thn) (doc-of-e els))))
                  ((F-set! x)
                   (doc-set! (doc-of-x x) □))
                  ((P-def x d* e*)
                   (v-concat
                    (append*
                     (list
                      (list (doc-set! (doc-of-x x) □))
                      (map doc-of-def d*)
                      (map doc-of-e e*)))))
                  ((P-exp v* e*)
                   (v-concat
                    (append
                     (map doc-of-v v*)
                     (map doc-of-e e*))))))))
      (values doc-of-e doc-of-addr doc-of-f))))