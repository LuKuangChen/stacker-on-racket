#lang plait

(require "io.rkt")
(require "error.rkt")
(require "utilities.rkt")
(require (typed-in racket
                   [random : (Number -> Number)]
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [remove-duplicates : ((Listof 'a) -> (Listof 'a))]))

(define-type-alias CompiledProgram ((Listof (Id * Term)) * (Listof Term)))
(define-type Term
  (t-quote [v : Val])
  (t-var [x : Id])
  (t-fun [name : (Optionof Symbol)]
         [arg* : (Listof Id)]
         [def* : (Listof (Id * Term))]
         [body : Term])
  (t-app [fun : Term] [arg* : (Listof Term)])
  (t-let [bind* : (Listof (Id * Term))] [body : Term])
  (t-letrec [bind* : (Listof (Id * Term))] [body : Term])
  (t-set! [var : Id] [val : Term])
  (t-begin [prelude* : (Listof Term)] [result : Term])
  (t-if [cnd : Term] [thn : Term] [els : Term])
  (t-cond [cnd-thn* : (Listof (Term * Term))] [els : Term]))

(define (t-or t1 t2)
  (t-if t1
        (t-quote (v-bool #t))
        t2))
(define (t-and t1 t2)
  (t-if t1
        t2
        (t-quote (v-bool #f))))

(define-type-alias (Result 'a) 'a)
(define-type Heap
  (heap-heap [it : (Hashof HeapAddress (Number * HeapValue))]))
(define (empty-heap) : Heap
  (heap-heap (hash (list))))
(define the-heap-size 1000)
(define (find-heap-addr the-heap [base : Number]) : Number
  (let ([propose (+ base (random the-heap-size))])
    (type-case (Optionof (Number * HeapValue)) (hash-ref the-heap (ha-user propose))
      [(none) propose]
      [(some hv) (find-heap-addr the-heap base)])))
(define (base-addr [hv : HeapValue])
  (type-case HeapValue hv
    [(h-env env map) the-heap-size]
    [else 0]))
(define (allocate! [a-heap : Heap] [hv : HeapValue]) : (Heap * HeapAddress)
  (let* ([a-heap (heap-heap-it a-heap)]
         [addr (ha-user (find-heap-addr a-heap (base-addr hv)))]
         [a-heap (hash-set a-heap addr (pair (length (hash-keys a-heap)) hv))])
    (values (heap-heap a-heap) addr)))
(define (heap-ref a-heap a-heap-address) : HeapValue
  (type-case (Optionof (Number * HeapValue)) (hash-ref (heap-heap-it a-heap) a-heap-address)
    [(none)
     (raise (exn-internal 'heap-ref (format "Invalid address ~a" a-heap-address)))]
    [(some hv)
     (snd hv)]))
(define (heap-set [h : Heap] [ha : HeapAddress] [hv : HeapValue]) : Heap
  (let* ([h (heap-heap-it h)]
         [timestamp
          (type-case (Optionof (Number * HeapValue)) (hash-ref h ha)
            [(none) (length (hash-keys h))]
            [(some v) (fst v)])])
    (heap-heap (hash-set h ha (pair timestamp hv)))))

(define-type HeapAddress
  (ha-prim [it : PrimitiveHeapAddress])
  (ha-user [it : Number]))
(define-type PrimitiveHeapAddress
  (pa-empty)
  (pa-map)
  (pa-filter)
  (pa-memberp)
  (pa-foldl)
  (pa-foldr)
  (pa-andmap)
  (pa-ormap)
  (pa-base-env))
(define-type HeapValue
  (h-vec [it : (Vectorof Val)])
  (h-cons [it : (Val * Val)])
  (h-fun [env : Env] [name : (Optionof Symbol)] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term])
  (h-env [parent : Env] [map : (Hashof Id (Optionof Val))]))
(define-type Val
  (v-addr [it : HeapAddress])
  (v-prim [name : PrimitiveOp])
  (v-str [it : String])
  (v-num [it : Number])
  (v-char [it : Char])
  (v-bool [it : Boolean])
  (v-empty)
  (v-void))
(define (v-fun [the-heap : Heap] [env : Env] name arg* def* body)
  (let-values (((the-heap addr) (allocate! the-heap (h-fun env name arg* def* body))))
    (values the-heap (v-addr addr))))
(define (v-vec [the-heap : Heap] it)
  (let-values (((the-heap addr) (allocate! the-heap (h-vec it))))
    (values the-heap (v-addr addr))))
(define (v-cons [the-heap : Heap] it)
  (let-values (((the-heap addr) (allocate! the-heap (h-cons it))))
    (values the-heap (v-addr addr))))
(define (v-list [the-heap : Heap] it)
  (if (empty? it)
      (values the-heap (v-empty))
      (let-values (((the-heap v) (v-list the-heap (rest it))))
        (let-values (((the-heap addr) (allocate! the-heap (h-cons (pair (first it) v)))))
          (values the-heap (v-addr addr))))))

(define-type PrimitiveOp
  (po-not)
  (po-left)
  (po-right)
  (po-vlen)
  (po-string-length)
  (po-string-append)
  (po-string->list)
  (po-list->string)
  (po-eqp)
  (po-equalp)
  (po-zerop)
  (po-+)
  (po--)
  (po-*)
  (po-/)
  (po-<)
  (po->)
  (po-<=)
  (po->=)
  (po-=)
  (po-emptyp)
  (po-pairp)
  (po-mpair)
  (po-set-left!)
  (po-set-right!)
  (po-vref)
  (po-consp)
  (po-cons)
  (po-vset!)
  (po-mvec)
  (po-first)
  (po-rest)
  (po-list))

(define-type-alias Env (Optionof HeapAddress))
(define (env-declare the-heap [env : Env] x*)
  (env-extend/declare the-heap env (map (lambda (x) (values x (none))) x*)))
(define (env-extend the-heap [env : Env] [x&v* : (Listof (Id * Val))])
  (env-extend/declare the-heap env (map2 pair (map fst x&v*) (map some (map snd x&v*)))))
(define (env-extend/declare the-heap [env : Env] [x&v* : (Listof (Id * (Optionof Val)))]): (Heap * Env)
  (let ((x* (map fst x&v*)))
    (if (no-duplicates x*)
        (let-values (((the-heap addr) (allocate! the-heap (h-env env (hash x&v*)))))
          (values the-heap (some addr)))
        (raise (exn-rt "redeclare")))))

(define (no-duplicates x*)
  (= (length x*)
     (length (remove-duplicates x*))))
(define (env-set the-heap [env : Env] x v)
  (let ((addr (some-v env)))
    (type-case HeapValue (heap-ref the-heap addr)
      ((h-env env map)
       (type-case (Optionof 'a) (hash-ref map x)
         ((none)
          (env-set the-heap env x v))
         ((some _)
          (heap-set
           the-heap
           addr
           (h-env env (hash-set map x (some v))) ))))
      (else
       (raise (exn-internal 'env-set "This is impossible. The address is not an env."))))))
(define (env-lookup the-heap [env : Env] x)
  (type-case (Optionof HeapAddress) env
    ((none)
     (none))
    ((some addr)
     (env-lookup-1 the-heap addr x))))
(define (env-lookup-1 the-heap addr x)
  (type-case HeapValue (heap-ref the-heap addr)
    ((h-env env map)
     (type-case
         (Optionof (Optionof Val))
       (hash-ref map x)
       ((none) (env-lookup the-heap env x))
       ((some v)
        (type-case
            (Optionof Val)
          v
          ((none) (raise (exn-rt (format "use-before-set ~a" x))))
          ((some v) (some v))))))
    (else
     (raise (exn-internal 'env-lookup "Not an env.")))))

(define-type ECFrame
  (F-begin [e* : (Listof Term)] [e : Term])
  (F-app [v* : (Listof Val)] [e* : (Listof Term)])
  (F-let [xv* : (Listof (Id * Val))] [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-if [thn : Term] [els : Term])
  (F-set! [var : Id])
  (P-def [x : Id] [d* : (Listof (Id * Term))] [e* : (Listof Term)])
  (P-exp [v* : (Listof Val)] [e* : (Listof Term)]))
(define-type-alias ECtx (Listof ECFrame))
(define-type CtxAnn
  (ca-app [v : Val] [v* : (Listof Val)])
  (ca-let)
  (ca-letrec))
(define-type-alias Ctx (Env * ECtx * CtxAnn))
(define-type-alias Stack (Listof Ctx))

(define (uninteresting-variable? x)
  (member x builtins))
(define-values (base-heap base-env builtins)
  (let* ((the-heap (empty-heap))
         [x&v*  (ind-List
                 (list
                  (values 'string->list (po-string->list))
                  (values 'first (po-first))
                  (values 'rest (po-rest))
                  (values 'not (po-not))
                  (values 'left (po-left))
                  (values 'string-length (po-string-length))
                  (values 'string-append (po-string-append))
                  (values 'string->list (po-string->list))
                  (values 'list->string (po-list->string))
                  (values 'right (po-right))
                  (values 'vlen (po-vlen))
                  (values 'equal? (po-equalp))
                  (values 'eq? (po-eqp))
                  (values 'zero? (po-zerop))
                  (values '+ (po-+))
                  (values '- (po--))
                  (values '* (po-*))
                  (values '/ (po-/))
                  (values '< (po-<))
                  (values '> (po->))
                  (values '<= (po-<=))
                  (values '>= (po->=))
                  (values '= (po-=))
                  (values 'empty? (po-emptyp))
                  (values 'pair? (po-pairp))
                  (values 'mpair (po-mpair))
                  (values 'set-left! (po-set-left!))
                  (values 'set-right! (po-set-right!))
                  (values 'vref (po-vref))
                  (values 'cons (po-cons))
                  (values 'cons? (po-consp))
                  (values 'vset! (po-vset!))
                  (values 'mvec (po-mvec))
                  (values 'list (po-list)))
                 (list)
                 (λ (IH e)
                   (cons (values (fst e)
                                 (v-prim (snd e)))
                         IH)))]
         [base-env-map (hash (append
                              (map2 pair
                                    (map fst x&v*)
                                    (map some (map snd x&v*)))
                              (list (pair 'empty (some (v-empty)))
                                    (pair 'map (some (v-addr (ha-prim (pa-map)))))
                                    (pair 'filter (some (v-addr (ha-prim (pa-filter)))))
                                    (pair 'member? (some (v-addr (ha-prim (pa-memberp)))))
                                    (pair 'foldl (some (v-addr (ha-prim (pa-foldl)))))
                                    (pair 'foldr (some (v-addr (ha-prim (pa-foldr)))))
                                    (pair 'andmap (some (v-addr (ha-prim (pa-andmap)))))
                                    (pair 'ormap (some (v-addr (ha-prim (pa-ormap)))))
                                    (pair 'false (some (v-bool #f)))
                                    (pair 'true (some (v-bool #t))))))]
         [builtins (hash-keys base-env-map)]
         [addr (ha-prim (pa-base-env))]
         [hv (h-env (none) base-env-map)]
         [the-heap (heap-set the-heap addr hv)]
         [base-env (some addr)]
         (the-heap (heap-set the-heap (ha-prim (pa-map))
                             (h-fun base-env
                                    (some 'map)
                                    (list 'f 'xs)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'xs) (t-var 'empty)))
                                          (t-var 'empty)
                                          (t-app (t-var 'cons)
                                                 (list (t-app (t-var 'f)
                                                              (list (t-app (t-quote (v-prim (po-first)))
                                                                           (list (t-var 'xs)))))
                                                       (t-app (t-var 'map)
                                                              (list (t-var 'f)
                                                                    (t-app (t-quote (v-prim (po-rest)))
                                                                           (list (t-var 'xs)))))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-memberp))
                             (h-fun base-env
                                    (some 'member?)
                                    (list 'x 'l)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'l) (t-var 'empty)))
                                          (t-quote (v-bool #f))
                                          (t-or (t-app (t-var 'equal?)
                                                      (list (t-app (t-quote (v-prim (po-first)))
                                                                           (list (t-var 'l)))
                                                            (t-var 'x)))
                                                (t-app (t-var 'member?)
                                                      (list (t-var 'x)
                                                            (t-app (t-quote (v-prim (po-rest)))
                                                                    (list (t-var 'l))))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-filter))
                             (h-fun base-env
                                    (some 'filter)
                                    (list 'f 'xs)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'xs) (t-var 'empty)))
                                          (t-var 'empty)
                                          (t-if (t-app (t-var 'f)
                                                       (list (t-app (t-quote (v-prim (po-first)))
                                                                    (list (t-var 'xs)))))
                                                (t-app (t-var 'cons)
                                                       (list (t-app (t-quote (v-prim (po-first)))
                                                                    (list (t-var 'xs)))
                                                             (t-app (t-var 'filter)
                                                                    (list (t-var 'f)
                                                                          (t-app (t-quote (v-prim (po-rest)))
                                                                                 (list (t-var 'xs)))))))
                                                (t-app (t-var 'filter)
                                                       (list (t-var 'f)
                                                             (t-app (t-quote (v-prim (po-rest)))
                                                                    (list (t-var 'xs))))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-foldl))
                             (h-fun base-env
                                    (some 'foldl)
                                    (list 'f 'base 'l)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'l) (t-var 'empty)))
                                          (t-var 'base)
                                          (t-app (t-var 'foldl)
                                                 (list
                                                  (t-var 'f)
                                                  (t-app (t-var 'f)
                                                         (list
                                                           (t-app (t-quote (v-prim (po-first)))
                                                                  (list (t-var 'l)))
                                                           (t-var 'base)))
                                                  (t-app (t-quote (v-prim (po-rest)))
                                                         (list (t-var 'l)))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-foldr))
                             (h-fun base-env
                                    (some 'foldr)
                                    (list 'f 'base 'l)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'l) (t-var 'empty)))
                                          (t-var 'base)
                                          (t-app (t-var 'f)
                                                 (list
                                                  (t-app (t-quote (v-prim (po-first)))
                                                         (list (t-var 'l)))
                                                  (t-app (t-var 'foldr)
                                                         (list
                                                           (t-var 'f)
                                                           (t-var 'base)
                                                           (t-app (t-quote (v-prim (po-rest)))
                                                                  (list (t-var 'l)))))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-andmap))
                             (h-fun base-env
                                    (some 'andmap)
                                    (list 'p? 'l)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'l) (t-var 'empty)))
                                          (t-quote (v-bool #t))
                                          (t-and (t-app (t-var 'p?)
                                                        (list
                                                          (t-app (t-quote (v-prim (po-first)))
                                                                (list (t-var 'l)))))
                                                 (t-app (t-var 'andmap)
                                                        (list
                                                          (t-var 'p?)
                                                          (t-app (t-quote (v-prim (po-rest)))
                                                                 (list (t-var 'l))))))))))
         (the-heap (heap-set the-heap (ha-prim (pa-ormap))
                             (h-fun base-env
                                    (some 'andmap)
                                    (list 'p? 'l)
                                    (list)
                                    (t-if (t-app (t-var 'equal?) (list (t-var 'l) (t-var 'empty)))
                                          (t-quote (v-bool #t))
                                          (t-or (t-app (t-var 'p?)
                                                        (list
                                                          (t-app (t-quote (v-prim (po-first)))
                                                                (list (t-var 'l)))))
                                                 (t-app (t-var 'andmap)
                                                        (list
                                                          (t-var 'p?)
                                                          (t-app (t-quote (v-prim (po-rest)))
                                                                 (list (t-var 'l))))))))))
         )
    (values the-heap base-env builtins)))

(define-type Operator
  (op-prim [name : PrimitiveOp])
  (op-fun [env : Env] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term]))

(define-type OtherState
  (calling [fun : Val] [args* : (Listof Val)] [env : Env] [ectx : ECtx] [stack : Stack]
           [clos-env : Env] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term])
  (called [e : Term] [env : Env] [stack : Stack])
  (returned [v : Val] [env : Env] [ectx : ECtx] [stack : Stack])
  (terminated [v* : (Listof Val)])
  (errored))
(define-type-alias State (Heap * OtherState))
