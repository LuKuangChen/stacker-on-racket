#lang plait

(require "utilities.rkt")
(require "datatypes.rkt")
(require
  (opaque-type-in racket
                  (Port port?))
  (typed-in racket
            (open-output-string : (-> Port))
            (get-output-string : (Port -> String))
            (write : ('a Port -> Void))
            (append* : ((Listof (Listof 'a)) -> (Listof 'a)))
            (vector->list : ((Vectorof 'a) -> (Listof 'a)))))
(require (opaque-type-in pprint (Doc doc?))
         (typed-in pprint
                   (text : (String -> Doc))
                   (align : (Doc -> Doc))
                   (hang : (Number Doc -> Doc))
                   (v-concat : ((Listof Doc) -> Doc))
                   (v-append : (Doc Doc -> Doc))
                   (vsb-concat : ((Listof Doc) -> Doc))
                   (h-concat : ((Listof Doc) -> Doc))
                   (hs-concat : ((Listof Doc) -> Doc))
                   (pretty-format : (Doc -> String))))
(require (typed-in "show.rkt" [string-of-o : (Obs -> String)]))
(require (typed-in racket
                   [number->string : (Number -> String)]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [sort : ((Listof 'x) ('x 'x -> Boolean) -> (Listof 'x))]
                   [append* : ((Listof (Listof 'x)) -> (Listof 'x))]))
(require (opaque-type-in racket [Any any/c]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))

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
     (hang 1 (v-append (hs-concat head-element*) body))))))


(define (s-exp-of-state fun-addr?)
  (lambda (state)
    (let-values (((the-heap state) state))
      (local ((define (doc-of-ha [it : HeapAddress])
                (type-case HeapAddress it
                  [(ha-user it)
                   (text
                    (let ([printing (format "@~a" it)])
                      (type-case HeapValue (heap-ref the-heap (ha-user it))
                        ((h-fun env name arg* def* body)
                         (type-case (Optionof Symbol) name
                           ((some s)
                            (if fun-addr?
                                (string-append printing (format ".~a" s))
                                (format "@~a" s)))
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
                   (doc-of-ha it))
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
              (define (doc-begin e* e)
                (if (empty? e*)
                    e
                    (head-body
                      (list
                        (symbol "begin"))
                        (v-concat (append e* (list e))))))
              (define (doc-if e*)
                (doc-paren
                 (hs-concat
                  (list
                   (text "if")
                   (align (v-concat e*))))))
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
                   (doc-begin (map doc-of-e e*) (doc-of-e e))]
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
                     (doc-begin (cons □ (map doc-of-e e*)) (doc-of-e e)))
                    ((F-app v* e*)
                     (doc-app (append
                       (map doc-of-v v*)
                       (cons
                        □
                        (map doc-of-e e*)))))
                    ((F-let xv* x xe* body)
                     (doc-let
                       (append*
                          (list
                           (map doc-of-xv xv*)
                           (list (doc-brack-pair (doc-of-x x) □))
                           (map doc-of-xe xe*)))
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
                       (cons □ (map doc-of-e e*))))))))
              (define (s-exp-of-stack stack)
                (inj (map s-exp-of-sf stack)))
              (define (s-exp-of-heap heap)
                (let* ([heap (heap-heap-it heap)]
                       [interesting-items 
                        (map
                         (lambda (key)
                           (pair key (some-v (hash-ref heap key))))
                         (filter ha-user? (hash-keys heap)))]
                       [interesting-items
                        (sort interesting-items
                              (lambda (item1 item2)
                                (< (fst (snd item1))
                                   (fst (snd item2)))))])
                  (inj
                   (map
                    (lambda (item)
                      (let-values ([(ha timestamp&hv) item])
                        (inj (list (inj (string-of-ha ha)) (s-exp-of-hv (snd timestamp&hv))))))
                    interesting-items))))
              (define (doc-of-ca ca)
                (type-case CtxAnn ca
                  [(ca-app v v*)
                   (doc-app (map doc-of-v (cons v v*)))]
                  [(ca-let)
                   (text "(let ...)")]
                  [(ca-letrec)
                   (text "(letrec ...)")]))
              (define (s-exp-of-hv [hv : HeapValue]) : Any
                (type-case HeapValue hv
                  [(h-vec it)
                   (inj (cons "vec" (map string-of-v (vector->list it))))]
                  [(h-cons it)
                   (inj (list "cons" (string-of-v (fst it)) (string-of-v (snd it))))]
                  [(h-fun env name arg* def* body)
                   (inj (list "fun" (string-of-env env) (string-of-e (t-fun name arg* def* body))))]
                  [(h-env env binding*)
                   (inj
                   (list
                     (inj "env")
                     (inj (string-of-env env)) 
                     (inj 
                       (ind-List (hash-keys binding*)
                        (list)
                        (lambda (IH x)
                          (cons 
                            (list
                              (string-of-x x)
                              (string-of-optionof-v (some-v (hash-ref binding* x))))
                            IH))))))]))
              (define (string-of-optionof-v ov)
                (type-case (Optionof '_) ov
                  [(none)
                   "💣"]
                  [(some v)
                  (string-of-v v)]))
              (define (string-of-env env)
                (type-case (Optionof '_) env
                  [(none)
                   "💣"]
                  [(some ha)
                  (string-of-ha ha)]))
              (define (s-exp-of-sf sf)
                (local ((define-values (env ectx ann) sf))
                  (inj (list (string-of-env env) (string-of-ectx ectx) (string-of-ann ann)))))
              (define (string-of-ann ann)
                (pretty-format (doc-of-ca ann)))
              (define (string-of-x x)
                (pretty-format (doc-of-x x)))
              (define (string-of-e e)
                (pretty-format (doc-of-e e)))
              (define (string-of-ha ha)
                (pretty-format (doc-of-ha ha)))
              (define (string-of-v v)
                (pretty-format (doc-of-v v)))
              (define (string-of-ectx ectx)
                (pretty-format
                  (ind-List (reverse (map doc-of-f ectx))
                    (text "□")
                    (lambda (IH x)
                      (x IH))))))
        (type-case OtherState state
          [(calling fun arg* env ectx stack clos-env arg-x* def* body)
           (inj (list (inj "calling")
                      (inj (string-of-e (t-app (t-quote fun) (map t-quote arg*))))
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(called e env stack)
           (inj (list (inj "called")
                      (inj (string-of-e e))
                      (inj (string-of-env env))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(returned v env ectx stack)
           (inj (list (inj "returned")
                      (inj (string-of-v v))
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(terminated v*)
           (inj (list (inj "terminated")
                      (inj (map string-of-v v*))
                      (s-exp-of-heap the-heap)))]
          [(errored)
           (inj (list (inj "errored")
                      (s-exp-of-heap the-heap)))])))))