#lang racket
(provide pict-of-state)
(require pict)
(require (rename-in pict [text pict-text]))
(require pict/color)
(require racket/draw)
(require "./string-of-state.rkt")

(define (text s)
  (apply vl-append (map (lambda (s) (pict-text s 'modern)) (string-split s "\n"))))

(define (pict-of-state hide-closure?)


  (define (pict-of-focus focus)
    (match focus
      [`("Terminated")
       (box (field-label "Terminated") "black")]
      [`("Computing" ,term)
       (box (field "Computing" term) "orange")]
      [`("Returning" ,term)
       (box (field "Returning" term) "brown")]))

  (define (pict-of-state state)
    (define p(match state
               [`("Terminated" ,heap)
                (bg "white"
                    (ht-append padding
                               (vl-append padding
                                          (pict-of-stack empty)
                                          (pict-of-focus `("Terminated")))
                               (pict-of-heap heap)))]
               [`(,message ,term ,env ,ectx ,stack ,heap)
                (bg "white"
                    (ht-append padding
                               (vl-append padding
                                          (pict-of-stack (cons (list env ectx term) stack))
                                          (pict-of-focus `(,message ,term)))
                               (pict-of-heap heap)))]))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (/ 600 dim)))

  (define (pict-of-stack stack)
    (box
     (apply vl-append
            (field-label "Stack")
            (map pict-of-sf (reverse stack)))
     "blue"))

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
    (apply vl-append padding
           (map pict-of-heapitem (filter heapitem-interesting? heapitems))))
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
               (white (text "Environment Frame"))
               (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key (compose symbol->string car))))))
               (field "Rest" outer-addr))
              (color-of-environment this-addr))]
      [`(Closure ,env ,name ,code)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-label "Closure")
                         (field "Environment @" env)
                         (field "Code" (string-of-s-exp code)))
              "blue")]
      [`,vec
       #:when (vector? vec)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "mvec" (apply hb-append padding (map field-value (vector->list vec)))))
              "blue")]
      [`(Cons ,v1 ,v2)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2)))))
              "blue")]
      [else
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "cons" hv))
              "blue")]))
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

  (define color-of-sf
    "royalblue")
  (define (pict-of-sf sf)
    (match-define (list env ectx ann) sf)
    (bg color-of-sf
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