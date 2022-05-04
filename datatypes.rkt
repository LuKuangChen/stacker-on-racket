#lang plait
(require "io.rkt")
(require (typed-in racket
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [vector-map : (('a -> 'b) (Vectorof 'a) -> (Vectorof 'b))]
                   [remove-duplicates : ((Listof 'a) -> (Listof 'a))]
                   [raise : (Exception -> 'a)]))
(require (typed-in "error.rkt"
                   [catch : ((-> 'a) (Exception -> 'a) -> 'a)]))

(define (ind-List (x* : (Listof 'a)) (base : 'b) (step : ('b 'a -> 'b)))
  (foldr (λ (x IH) (step IH x)) base x*))
(define-type Exception
  (exn-tc [msg : String])
  (exn-rt [msg : String])
  (exn-internal [where : Symbol] [what : String]))

(define-type Term
  (t-quote [v : Val])
  (t-var [x : Id])
  (t-fun [arg* : (Listof Id)] [body : Term])
  (t-app [fun : Term] [arg* : (Listof Term)])
  (t-let [bind* : (Listof (Id * Term))] [body : Term])
  (t-letrec [bind* : (Listof (Id * Term))] [body : Term])
  (t-letrec-1 [bind* : (Listof (Id * Term))] [body : Term])
  (t-set! [var : Id] [val : Term])
  (t-begin [prelude* : (Listof Term)] [result : Term])
  (t-if [cnd : Term] [thn : Term] [els : Term])
  (t-show [val : Term]))

(define-type-alias (Result 'a) 'a)
(define-type HeapAddress
  (ha-prim [it : String])
  (ha-user [it : Number]))
(define-type-alias Heap (Hashof HeapAddress HeapValue))
(define-type HeapValue
  (h-vec [it : (Vectorof Val)])
  (h-list [it : (Listof Val)])
  (h-fun [env : Env] [arg* : (Listof Id)] [body : Term])
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
    addr))
(define (v-fun env arg* body)
  (let ([addr (allocate! (h-fun env arg* body))])
    (v-addr addr)))
(define (v-vec it)
  (let ([addr (allocate! (h-vec it))])
    (v-addr addr)))
(define (v-list it)
  (let ([addr (allocate! (h-list it))])
    (v-addr addr)))

(define-type PrimitiveOp
  (po-+)
  (po--)
  (po-*)
  (po-/)
  (po-pairp)
  (po-pair)
  (po-left)
  (po-right)
  (po-ivec)
  (po-list)
  (po-equalp))

(define-type-alias Env (Optionof HeapAddress))
(define (env-declare [env : Env] x*): Env
  (if (no-duplicates x*)
      (let ((addr (allocate! (h-env env (make-hash (map (λ (x) (pair x (none))) x*))))))
        (some addr))
      (raise (exn-rt "redeclare"))))
(define (env-extend [env : Env] [x&v* : (Listof (Id * Val))]): Env
  (some (allocate! (h-env env
                          (make-hash (map2 pair
                                           (map fst x&v*)
                                           (map some (map snd x&v*))))))))
(define (base-env)
  (let* ([env (none)]
         [x&v*  (ind-List
                (list (values 'equal? (po-equalp))
                      (values '+ (po-+))
                      (values '- (po--))
                      (values '* (po-*))
                      (values '/ (po-/))
                      (values 'pair? (po-pairp))
                      (values 'pair (po-pair))
                      (values 'left (po-left))
                      (values 'right (po-right))
                      (values 'ivec (po-ivec))
                      (values 'list (po-list)))
                (list)
                (λ (IH e)
                  (cons (values (fst e)
                                (v-prim (snd e)))
                        IH)))]
         [addr (ha-prim "base-env")]
         [hv (h-env env  (make-hash (map2 pair
                                          (map fst x&v*)
                                          (map some (map snd x&v*)))))]
         [_ (hash-set! the-heap addr hv)])
    (some addr)))
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
          ((none) (raise (exn-rt "(use-before-set x")))
          ((some v) v)))))
    (else
     (raise (exn-internal 'env-lookup "Not an env.")))))
(define (base-Tenv)
  (hash-set* (hash '())
             (list (values 'equal? (T-fun))
                   (values '+ (T-fun))
                   (values '- (T-fun))
                   (values '* (T-fun))
                   (values '/ (T-fun))
                   (values 'pair? (T-fun))
                   (values 'pair (T-fun))
                   (values 'left (T-fun))
                   (values 'right (T-fun))
                   (values 'ivec (T-fun))
                   (values 'list (T-fun)))))
(define-type Type
  (T-val)
  (T-fun))
(define (hash-set* base ⟨k×v⟩*)
  (ind-List ⟨k×v⟩*
            base
            (λ (IH ⟨k×v⟩)
              (hash-set IH (fst ⟨k×v⟩) (snd ⟨k×v⟩)))))
(define-type ECFrame
  (F-begin [e* : (Listof Term)] [e : Term])
  (F-app [v* : (Listof Val)] [e* : (Listof Term)])
  (F-show!)
  (F-let [xv* : (Listof (Id * Val))] [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-letrec-1 [x : Id] [xe* : (Listof (Id * Term))] [body : Term])
  (F-if [thn : Term] [els : Term])
  (F-set! [var : Id]))
(define-type-alias ECtx (Listof ECFrame))
(define-type-alias Ctx (Env * ECtx))
(define-type-alias Stack (Listof Ctx))