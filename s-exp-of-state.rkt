#lang plait

(require "utilities.rkt")
(require "datatypes.rkt")
(require "error.rkt")
(require "io.rkt")
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
                   (group : (Doc -> Doc))
                   (align : (Doc -> Doc))
                   (hang : (Number Doc -> Doc))
                   (v-concat : ((Listof Doc) -> Doc))
                   (v-append : (Doc Doc -> Doc))
                   (vs-concat : ((Listof Doc) -> Doc))
                   (h-concat : ((Listof Doc) -> Doc))
                   (hs-concat : ((Listof Doc) -> Doc))
                   (pretty-format : (Doc -> String))))
(require
  (rename-in (typed-in pprint [pretty-format : (Doc Number -> String)])
             [pretty-format pretty-format/n]))
(require (typed-in "show.rkt" [string-of-o : (Obs -> String)]))
(require (typed-in racket
                   [number->string : (Number -> String)]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [sort : ((Listof 'x) ('x 'x -> Boolean) -> (Listof 'x))]
                   [append* : ((Listof (Listof 'x)) -> (Listof 'x))]))
(require (opaque-type-in racket [Any any/c]))
(require (rename-in (typed-in racket [identity : ('a -> Any)]) [identity inj]))


(define (string-of-o-of-v the-heap)
  (let ([obs (obs-of-val the-heap)])
    (lambda (v)
      (string-of-o (obs v)))))
(define (obs-of-val the-heap)
  (lambda (v)
  (local ([define counter 0]
          (define visited (make-hash (list)))
          (define (obs-of-hv hv)
            (type-case HeapValue hv
              ((h-vec vs) (o-vec (vector-map obs-of-val vs)))
              ((h-cons vs) (o-list (cons (obs-of-val (fst vs))
                                        (o-list-it (obs-of-val (snd vs))))))
              ((h-fun env name arg* body)
               (o-fun (type-case (Optionof '_) name
                        [(none) (none)]
                        [(some x) (some (symbol->string x))])))
              ((h-env _env _map)
              (raise (exn-internal 'obs-of-val "Impossible.")))))
          [define obs-of-val
            (lambda (v)
              (type-case
                  Val
                v
                ((v-str it) (o-con (c-str it)))
                ((v-num it) (o-con (c-num it)))
                ((v-bool it) (o-con (c-bool it)))
                ((v-char it) (o-con (c-char it)))
                ((v-prim name) (o-fun (some (pretty-format (doc-of-prim name)))))
                ((v-empty) (o-list '()))
                ((v-void) (o-void))
                ((v-addr addr)
                 (type-case (Optionof (Optionof Number)) (hash-ref visited addr)
                   [(none)
                    (begin
                      (hash-set! visited addr (none))
                      (let ([o (obs-of-hv (heap-ref the-heap addr))])
                        (type-case (Optionof Number) (some-v (hash-ref visited addr))
                          [(none) o]
                          [(some id) (o-rec id o)])))]
                   [(some optionof-id)
                    (begin
                      (when (equal? optionof-id (none))
                        (hash-set! visited addr (some counter))
                        (set! counter (add1 counter)))
                      (o-var (some-v (some-v (hash-ref visited addr)))))]))))])
    (obs-of-val v))))

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
    [(po-vec-len)
     (doc-of-x 'vec-len)]
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
    [(po-vec-ref)
     (doc-of-x 'vec-ref)]
    [(po-cons)
     (doc-of-x 'cons)]
    [(po-first)
     (doc-of-x 'first)]
    [(po-rest)
     (doc-of-x 'rest)]
    [(po-vec-set!)
     (doc-of-x 'vec-set!)]
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


(define (s-exp-of-state hide-fun-addr? defvar-lambda-as-deffun? set!-lambda-as-def? set!-other-as-def?)
  (lambda (state)
    (let-values (((the-heap state) state))
      (local ((define (stringify-ha [it : HeapAddress]) : String
                (type-case HeapAddress it
                  [(ha-user it)
                   (let ([printing (format "~a" it)])
                      (type-case HeapValue (heap-ref the-heap (ha-user it))
                        ((h-fun env name arg* body)
                         (type-case (Optionof Symbol) name
                           ((some s)
                            (if hide-fun-addr?
                                (format "~a" s)
                                (string-append printing (format ".~a" s))))
                           ((none)
                            printing)))
                        (else
                         printing)))]
                  [(ha-prim it)
                   (string-of-primitive-address it)]))
              (define (doc-of-ha ha)
                (text (stringify-ha ha)))
              (define (string-of-primitive-address pa)
                (type-case PrimitiveHeapAddress pa
                  [(pa-map) (symbol->string 'map)]
                  [(pa-filter) (symbol->string 'filter)]
                  [(pa-memberp) (symbol->string 'member?)]
                  [(pa-foldl) (symbol->string 'foldl)]
                  [(pa-foldr) (symbol->string 'foldr)]
                  [(pa-andmap) (symbol->string 'andmap)]
                  [(pa-ormap) (symbol->string 'ormap)]
                  [(pa-base-env) (symbol->string 'primordial-env)]
                  [(pa-empty) (symbol->string 'empty)]))
              (define (doc-of-v v)
                (type-case Val v
                  ((v-addr it)
                   (h-concat (list (text "@") (doc-of-ha it))))
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
                   (text "#<void>"))))
              (define (doc-of-def def)
                (local ((define-values (x e) def))
                  (if (and defvar-lambda-as-deffun? (t-fun? e))
                      (doc-of-deffun x
                        (t-fun-arg* e)
                        (t-fun-body e))
                      (group
                        (head-body
                          (list
                            (symbol "defvar")
                            (doc-of-x x))
                          (doc-of-e e))))))
              (define (doc-of-body [body : Block]) : Doc
                (let ([def* (block-def* body)]
                      [exp* (block-exp* body)]
                      [out (block-out body)])
                  (v-concat
                    (append
                      (map doc-of-def def*)
                      (append 
                        (map doc-of-e exp*)
                        (list (doc-of-e out)))))))
              (define (doc-of-deffun x arg* [body : Block])
                (head-body
                  (list
                    (symbol "deffun")
                    (doc-list
                      (cons (doc-of-x x)
                        (map doc-of-x arg*))))
                  (doc-of-body body)))
              (define (doc-lambda arg* [body : Doc])
                (head-body
                 (list
                  (symbol "lambda")
                  (doc-list arg*))
                 body))
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
                (group
                  (if set!-other-as-def?
                    (head-body
                      (list
                        (symbol "defvar")
                        x)
                      e)
                    (head-body
                      (list
                        (symbol "set!")
                        x)
                      e))))
              (define (doc-of-set! x e)
                (cond
                  [(and set!-lambda-as-def? (t-fun? e))
                   (doc-of-def (values x e))]
                  [(and set!-other-as-def? (not (t-fun? e)))
                   (doc-of-def (values x e))]
                  [else
                   (group 
                     (head-body
                       (list
                         (symbol "set!")
                        (doc-of-x x))
                      (doc-of-e e)))]))
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
              (define (doc-of-e-top e) : Doc
                (type-case Term e
                  [(t-begin e* e)
                   (v-concat (map doc-of-e (append e* (list e))))]
                  [else
                   (doc-of-e e)]))
              (define (doc-of-e e) : Doc
                (type-case Term e
                  [(t-quote v)
                   (doc-of-v v)]
                  [(t-var x)
                   (doc-of-x x)]
                  [(t-fun name arg* body)
                   (doc-lambda
                    (map doc-of-x arg*)
                    (doc-of-body body))]
                  [(t-app fun arg*)
                   (doc-app
                    (map doc-of-e (cons fun arg*)))]
                  [(t-let bind* body)
                   (doc-let
                    (map doc-of-xe bind*)
                    (doc-of-body body))]
                  [(t-letrec bind* body)
                   (doc-letrec
                    (map doc-of-xe bind*)
                    (doc-of-body body))]
                  [(t-set! x e)
                   (doc-of-set! x e)]
                  [(t-begin e* e)
                   (doc-begin (map doc-of-e e*) (doc-of-e e))]
                  [(t-if cnd thn els)
                   (doc-if (map doc-of-e (list cnd thn els)))]
                  [(t-cond cnd-thn* els)
                   (doc-cond
                    (map doc-of-ee
                         (append
                          cnd-thn*
                          (type-case (Optionof '_) els
                            [(none) (list)]
                            [(some e)
                             (list (values (t-var 'else) e))]))))]))
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
                        (doc-of-body body)))
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
                  [(h-fun env name arg* body)
                   (inj (list "fun" (string-of-env env) (string-of-e (t-fun name arg* body))))]
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
                (pretty-format/n (doc-of-e e) 20))
              (define (string-of-e-top body)
                (begin
                  (pretty-format/n (doc-of-e-top body) 20)))
              (define (string-of-ha ha)
                (pretty-format (doc-of-ha ha)))
              (define (string-of-v v)
                (pretty-format (doc-of-v v)))
              (define (string-of-ectx ectx)
                (pretty-format/n
                  (ind-List (reverse (map doc-of-f ectx))
                    (text "□")
                    (lambda (IH x)
                      (x IH)))
                  20)))
        (type-case OtherState state
          [(vector-setting addr it i velm env ectx stack)
           (inj (list (inj "vec-setting")
                      (inj (string-of-e (t-app (t-quote (v-prim (po-vec-set!)))
                                               (list
                                                 (t-quote (v-addr addr))
                                                 (t-quote (v-num i))
                                                 (t-quote velm)))))
                      #;(inj (string-of-ha addr))
                      #;(inj (number->string i))
                      #;(inj (string-of-v velm))
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(setting x v env ectx stack)
           (inj (list (inj "setting")
                      (inj (string-of-x x))
                      (inj (string-of-v v))
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(setted env ectx stack)
           (inj (list (inj "setted")
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(calling fun arg* env ectx stack clos-env arg-x* body)
           (inj (list (inj "calling")
                      (inj (string-of-e (t-app (t-quote fun) (map t-quote arg*))))
                      (inj (string-of-env env))
                      (inj (string-of-ectx ectx))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(called body env stack)
           (inj (list (inj "called")
                      (inj (string-of-e-top body))
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
          [(returning v stack)
           (inj (list (inj "returning")
                      (inj (string-of-v v))
                      (s-exp-of-stack stack)
                      (s-exp-of-heap the-heap)))]
          [(terminated v*)
           (inj (list (inj "terminated")
                      (inj (map string-of-v v*))
                      (s-exp-of-heap the-heap)))]
          [(errored)
           (inj (list (inj "errored")
                      (s-exp-of-heap the-heap)))])))))