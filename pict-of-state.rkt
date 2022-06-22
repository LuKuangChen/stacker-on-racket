#lang racket
(provide pict-of-state)
(require pict)
(require (rename-in pict [text pict-text]))
(require pict/color)
(require racket/draw)

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
  (if (equal? s "")
      (pict-text " ")
      (apply vl-append (map (lambda (s) (pict-text s 'modern)) (string-split s "\n")))))

(define (pict-of-state hide-closure? hide-env-lable?)
  (define color-comp color-closure)
  (define color-return color-stack-item)
  (define color-terminate color-stack-bg)
  (define color-refer color-other)
  (define (pict-of-focus focus)
    (match focus
      [`("calling" ,app ,env ,ectx)
       (plate (vl-append padding
                         (field "Calling" app)
                         (field "Context" ectx)
                         (field "Environment @" env))
              color-comp)]
      [`("called" ,body ,env)
       (plate (vl-append padding
                         (field-label "Computing")
                         (field-value body)
                         (field "Environment @" env))
              color-comp)]
      [`("returned" ,v ,env ,ectx)
       (plate (vl-append padding
                         (field "Returned" v)
                         (field "Context" ectx)
                         (field "Environment @" env))
              color-return)]
      [`("terminated" ,v*)
       (plate (vl-append padding
                         (field-label "Terminated")
                         (field-value (string-join v* "\n")))
              color-terminate)]
      [`("errored" ,v*)
       (plate (vl-append padding
                         (field-label "Errored"))
              color-error)]))

  (define (main-pict stack focus heap)
    (bg "white"
        (ht-append padding
                   (vl-append padding
                              (pict-of-stack stack)
                              (pict-of-focus focus))
                   (pict-of-heap heap))))

  (define (pict-of-state state)
    (define p
      (match state
        [`("calling" ,app ,env ,ectx ,stack ,heap)
         (main-pict stack `("calling" ,app ,env ,ectx) heap)]
        [`("called" ,body ,env ,stack ,heap)
         (main-pict stack `("called" ,body ,env) heap)]
        [`("returned" ,v ,env ,ectx ,stack ,heap)
         (main-pict stack `("returned" ,v ,env ,ectx) heap)]
        [`("terminated" ,v* ,heap)
         (main-pict empty `("terminated" ,v*) heap)]
        [`("errored" ,heap)
         (main-pict empty `("errored") heap)]))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (min (/ 700 (pict-height p)) (/ 1200 (pict-width p)))))

  (define (pict-of-stack stack)
    (box
     (apply vl-append
            (field-label "Stack")
            (map pict-of-sf (reverse stack)))
     color-stack-bg))

  (define (is-env? heapitem)
    (match-define (list addr hv) heapitem)
    (match hv
      [`("env" ,@_)
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
      [`("env" ,env ,bindings)
       (plate (vl-append
               (field "@" this-addr)
               (if hide-env-lable?
                   (blank)
                   (white (text "Environment Frame")))
               (field-pict "bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key (compose symbol->string car))))))
               (field "Rest @" env))
              color-env)]
      [`("fun" ,env ,code)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "Environment @" env)
                         (field "Code" code))
              color-closure)]
      [`("vec" ,vec)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "mvec" (apply hb-append padding (map field-value vec))))
              color-vector)]
      [`("cons" ,v1 ,v2)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2)))))
              color-cons)]))
  (define (plate p color)
    (define w (pict-width p))
    (define h (pict-height p))
    (define r 10)
    (cc-superimpose
     (filled-rounded-rectangle
       (+ (pict-width p) (* r 2))
       (+ (pict-height p) (* r 2))
       r
       #:color color)
     p))

  (define (box p color)
    (frame (bg color (pad padding p))))
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
    (bg "white" (text value)))
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