#lang racket
(provide pict-of-state)
(require pict)
(require (rename-in pict [text pict-text]))
(require pict/color)
(require racket/draw)
(require "./string-of-state.rkt")

(define color-blue (make-object color% 68 119 170))
(define color-cyne (make-object color% 102 204 238))
(define color-green (make-object color% 34 136 51))
(define color-yellow (make-object color% 204 187 68))
(define color-black (make-object color% 0 0 0))
(define color-purple (make-object color% 170 51 119))
(define color-grey (make-object color% 187 187 187))
(define color-red (make-object color% 238 102 119))

;;; The color palette is from https://personal.sron.nl/~pault/#fig:scheme_bright
;;; dark blue
(define color-stack-item color-blue)
;;; light blue
(define color-stack-bg color-cyne)
;;; green
(define color-env color-green)
;;; yellow
(define color-closure color-yellow)
;;; black
(define color-vector color-black)
;;; purple
(define color-cons color-purple)
;;; grey
(define color-other color-grey)
;; A special color
(define color-error color-red)

(define (text s)
  (apply vl-append
    (map
      (lambda (s)
        (if (equal? s "#<void>")
            (pict-text "...void...")
            (pict-text s 'modern)))
      (string-split s "\n"))))

(define (pict-of-state hide-closure? hide-env-lable?)
  (define color-comp color-closure)
  (define color-return color-stack-item)
  (define color-terminate color-stack-bg)
  (define color-refer color-other)
  (define (pict-of-focus focus)
    (match focus
      [`("Computing" ,term)
       (box (field "Computing" term) color-comp)]
      [`("Returned" ,term)
       (box (field "Returned" term) color-return)]
      [`("Terminated" ,term)
       (box (field "Terminated" term) color-terminate)]
      [`("Referring to" ,term)
       (box (field "Returned" term) color-refer)]))

  (define (pict-of-state state)
    (define p (match state
                [`("Errored" ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack empty)
                                           (box (field-label "Errored") color-error))
                                (pict-of-heap heap)))]
                [`("Terminated" ,o* ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack empty)
                                           (pict-of-focus `("Terminated" (,block ,@o*))))
                                (pict-of-heap heap)))]
                [`(,message ,term ,env ,ectx ,stack ,heap)
                 (bg "white"
                     (ht-append padding
                                (vl-append padding
                                           (pict-of-stack (cons (list env ectx term) stack))
                                           (pict-of-focus `(,message ,term)))
                                (pict-of-heap heap)))]))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (/ 700 dim)))

  (define (pict-of-stack stack)
    (box
     (apply vl-append
            (field-label "Stack")
            (map pict-of-sf (reverse stack)))
     color-stack-bg))

  (define (is-env? heapitem)
    (match-define (list addr hv) heapitem)
    (match hv
      [`(Environment ,@_)
       #t]
      [else
       #f]))

  (define (pict-of-heap heap)
    (pict-of-heapitems heap))

  (define (heapitem-interesting? item)
    (match-define `(,this-addr ,hv) item)
    (and (string? this-addr) ;; string addresses means the address is not primitive (symbol address)
         (if hide-closure?
             (not (is-closure? hv))
             #t)))
  (define (is-closure? hv)
    (match hv
      [`(Closure ,@_) #t]
      [else #f]))

  (define (pict-of-heapitems heapitems)
    (let-values ([(envs others) (partition is-env?
                                           (filter heapitem-interesting? heapitems))])
      (ht-append
        padding
       (apply vl-append padding
              (map pict-of-heapitem envs))
       (apply vl-append padding
              (map pict-of-heapitem others)))))
  (define (color-of-environment addr)
    (define addr-as-num (string->number addr))
    (define ratio (/ (- addr-as-num 1000) 1000))
    (define green (* ratio 200))
    (make-object color% 0 (round green) 0))
  (define (pict-of-heapitem item)
    (match-define `(,this-addr ,hv) item)
    (match hv
      [`(Environment ,bindings ,outer-addr)
       (plate (vl-append
               (field "@" this-addr)
               (if hide-env-lable?
                   (blank)
                   (white (text "Environment Frame")))
               (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key (compose symbol->string car))))))
               (field "Rest @" outer-addr))
              color-env)]
      [`(Closure ,env ,name ,code)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "Environment @" env)
                         (field "Code" (string-of-s-exp code)))
              color-closure)]
      [`,vec
       #:when (vector? vec)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "mvec" (apply hb-append padding (map field-value (vector->list vec)))))
              color-vector)]
      [`(Cons ,v1 ,v2)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2)))))
              color-cons)]
      [else
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "content" hv))
              color-other)]))
  (define (plate p color)
    (define w (pict-width p))
    (define h (pict-height p))
    (cc-superimpose
     (filled-ellipse
      (* (sqrt 2) (+ (pict-width p) 6))
      (* (sqrt 2) (+ (pict-height p) 6))
      #:color color)
     p))

  (define (box p color)
    (frame (bg color (pad padding p))))
  (define (pict-of-envs all-env-heapitems)
    (define (rec root)
      (let ([env (car (dict-ref all-env-heapitems root))])
        (define children-pict
          (apply hb-append padding
                 (map rec (map addr-of-heapitem (filter (is-sub-env? root) all-env-heapitems)))))
        (define children-width (pict-width children-pict))
        (vc-append
         children-pict
         (pict-of-env children-width root env))))
    (rec '|@base-env|))
  (define (addr-of-heapitem item)
    (match-define (list addr hv) item)
    addr)
  (define ((is-sub-env? root-addr) heapitem)
    (match heapitem
      [`(,this-addr (Environment ,bindings ,outer-addr))
       (equal? root-addr outer-addr)]
      [else #f]))
  (define (pict-of-env children-width this-addr env)
    (match-define `(Environment ,bindings ,outer-addr) env)
    (define content-pict
      (vl-append
       (field "@" this-addr)
       (white (text "Environment Frame"))
       (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                  (field-value '...)
                                  (apply vl-append padding (map pict-of-binding bindings))))
       (field "Rest" outer-addr)))
    (define content-width (pict-width content-pict))
    (frame
     (bg "blue"
         (pad padding
              (ht-append content-pict (blank (max 0 (- children-width content-width)) 0))))))
  (define (pict-of-binding binding)
    (match-define (list x v) binding)
    (ht-append padding
               (field-value x)
               (field-label "↦")
               (field-value v)))

  (define padding 5)

  (define (field name value)
    (ht-append padding (white (text name)) (field-value value)))
  (define (field-label name)
    (white (text name)))
  (define (field-value value)
    (bg "white" (text (if (string? value) value (string-of-s-exp value)))))
  (define (field-pict name p)
    (ht-append padding (white (text name)) p))

  (define (bg color p)
    (cc-superimpose (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color color) p))

  (define (pict-of-sf sf)
    (match-define (list env ectx ann) sf)
    (bg color-stack-item
        (frame
         (pad padding
              (vl-append padding
                         (field "Context" ectx)
                         (field "Environment @" env))))))

  (define (pad n p)
    (hc-append (blank n)
               (vc-append (blank n) p (blank n))
               (blank n)))

  pict-of-state)