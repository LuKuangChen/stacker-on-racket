#lang plait
(require "io.rkt")
(require "error.rkt")
(require "utilities.rkt")
(require (typed-in racket
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [remove-duplicates : ((Listof 'a) -> (Listof 'a))]))

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
  (t-letrec-1 [bind* : (Listof (Id * Term))] [body : Term])
  (t-fun-call [bind* : (Listof (Id * Term))] [body : Term])
  (t-set! [var : Id] [val : Term])
  (t-begin [prelude* : (Listof Term)] [result : Term])
  (t-if [cnd : Term] [thn : Term] [els : Term])
  (t-show [val : Term]))

(define-type-alias (Result 'a) 'a)
(define-type HeapAddress
  (ha-prim [it : PrimitiveHeapAddress])
  (ha-user [it : Number]))
(define-type PrimitiveHeapAddress
  (pa-empty)
  (pa-base-env))
(define-type-alias Heap (Hashof HeapAddress HeapValue))
(define-type HeapValue
  (h-vec [it : (Vectorof Val)])
  (h-list [it : (Listof Val)])
  (h-fun [env : Env] [name : (Optionof Symbol)] [arg* : (Listof Id)] [def* : (Listof (Id * Term))] [body : Term])
  (h-env [parent : Env] [map : (Hashof Id (Optionof Val))]))
(define-type Val
  (v-addr [it : HeapAddress])
  (v-prim [name : PrimitiveOp])
  (v-str [it : String])
  (v-num [it : Number])
  (v-bool [it : Boolean])
  (v-void))
(define the-heap : Heap (make-hash (list)))
(define next-heap-addr : Number 0)
(define (allocate! h)
  (let* ([addr (ha-user next-heap-addr)]
         [_ (set! next-heap-addr (add1 next-heap-addr))]
         [_ (hash-set! the-heap addr h)])
    (begin
      (display "allocated ")
      (display h)
      (display " at ")
      (display addr)
      (display "\n")
      addr)))
(define (v-fun name env arg* def* body)
  (let ([addr (allocate! (h-fun env name arg* def* body))])
    (v-addr addr)))
(define (v-vec it)
  (let ([addr (allocate! (h-vec it))])
    (v-addr addr)))
(define (v-list it)
  (let ([addr (allocate! (h-list it))])
    (v-addr addr)))

(define-type PrimitiveOp
  (po-left)
  (po-right)
  (po-vlen)
  (po-equalp)
  (po-+)
  (po--)
  (po-*)
  (po-/)
  (po-pairp)
  (po-mpair)
  (po-set-left!)
  (po-set-right!)
  (po-vref)
  (po-cons)
  ;;; (po-map)
  ;;; (po-filter)
  (po-vset!)
  ;;; (po-foldl)
  ;;; (po-foldr)
  (po-mvec)
  (po-list)
  (po-pause))

(define-type-alias Env (Optionof HeapAddress))
(define (env-declare [env : Env] x*): Env
  (if (no-duplicates x*)
      (let ((addr (allocate! (h-env env (make-hash (map (λ (x) (pair x (none))) x*))))))
        (some addr))
      (raise (exn-rt "redeclare"))))
(define (env-extend [env : Env] [x&v* : (Listof (Id * Val))]): Env
  (env-extend/declare env (map2 pair (map fst x&v*) (map some (map snd x&v*)))))
(define (env-extend/declare [env : Env] [x&v* : (Listof (Id * (Optionof Val)))]): Env
  (some (allocate! (h-env env (make-hash x&v*)))))

(define (no-duplicates x*)
  (= (length x*)
     (length (remove-duplicates x*))))
(define (env-set! [env : Env] x v)
  (type-case (Optionof HeapAddress) env
    ((none)
     (raise (exn-internal 'env-set! "This is impossible")))
    ((some addr)
     (env-set!-1 addr x v))))
(define (env-set!-1 addr x v)
  (type-case HeapValue (some-v (hash-ref the-heap addr))
    ((h-env env map)
     (type-case (Optionof 'a) (hash-ref map x)
       ((none)
        (env-set! env x v))
       ((some _)
        (hash-set! map x (some v)))))
    (else
     (raise (exn-internal 'env-set! "This is impossible. The address is not an env.")))))
(define (env-lookup [env : Env] x)
  (type-case (Optionof HeapAddress) env
    ((none)
     (raise (exn-internal x "unbound id")))
    ((some addr)
     (env-lookup-1 addr x))))
(define (env-lookup-1 addr x)
  (type-case HeapValue (some-v (hash-ref the-heap addr))
    ((h-env env map)
     (type-case
         (Optionof (Optionof Val))
       (hash-ref map x)
       ((none) (env-lookup env x))
       ((some v)
        (type-case
            (Optionof Val)
          v
          ((none) (raise (exn-rt (format "use-before-set ~a" x))))
          ((some v) v)))))
    (else
     (raise (exn-internal 'env-lookup "Not an env.")))))
(define-type ECFrame
  (F-begin [e* : (Listof Term)] [e : Term])
  (F-app [v* : (Listof Val)] [e* : (Listof Term)])
  (F-show!)
  (F-let [xv* : (Listof (Id * Val))] [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-letrec-1 [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-fun-call [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-if [thn : Term] [els : Term])
  (F-set! [var : Id]))
(define-type-alias ECtx (Listof ECFrame))
(define-type CtxAnn
  (ca-app [v : Val] [v* : (Listof Val)])
  (ca-let)
  (ca-letrec))
(define-type-alias Ctx (Env * ECtx * CtxAnn))
(define-type-alias Stack (Listof Ctx))

(define base-env
  (let* ([env (none)]
         [x&v*  (ind-List
                 (list
                  (values 'left (po-left))
                  (values 'right (po-right))
                  (values 'vlen (po-vlen))
                  (values 'equal? (po-equalp))
                  (values '+ (po-+))
                  (values '- (po--))
                  (values '* (po-*))
                  (values '/ (po-/))
                  (values 'pair? (po-pairp))
                  (values 'mpair (po-mpair))
                  (values 'set-left! (po-set-left!))
                  (values 'set-right! (po-set-right!))
                  (values 'vref (po-vref))
                  (values 'cons (po-cons))
                  ;;; (values 'map (po-map))
                  ;;; (values 'filter (po-filter))
                  (values 'vset! (po-vset!))
                  ;;; (values 'foldl (po-foldl))
                  ;;; (values 'foldr (po-foldr))
                  (values 'mvec (po-mvec))
                  (values 'list (po-list))
                  (values 'pause (po-pause)))
                 (list)
                 (λ (IH e)
                   (cons (values (fst e)
                                 (v-prim (snd e)))
                         IH)))]
         [addr (ha-prim (pa-base-env))]
         [hv (h-env env  (make-hash (append
                                     (map2 pair
                                           (map fst x&v*)
                                           (map some (map snd x&v*)))
                                     (list (pair 'empty (some (v-addr (ha-prim (pa-empty)))))))))]
         [_ (hash-set! the-heap addr hv)])
    (some addr)))
