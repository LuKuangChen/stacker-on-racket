#lang racket
(provide pict-of-state)
(require pict)
(require (rename-in pict [text pict-text]))
(require pict/color)
(require "./string-of-state.rkt")

(define (text s)
  (apply vl-append (map (lambda (s) (pict-text s 'modern)) (string-split s "\n"))))

(define (pict-of-state hide-closure?)
  (define (stack-frame-of-pctx pctx)
    (match pctx
      [`(,ctx ,env)
       (list env ctx "top-level")]))

  (define (pict-of-state message term env ectx stack pctx heap)
    (define p (bg "black"
                  (ht-append padding
                             (vl-append padding
                                        (pict-of-stack (append stack (list (stack-frame-of-pctx pctx))))
                                        (pict-of-focus message term ectx env))
                             (pict-of-heap heap))))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (/ 600 dim)))

  (define (pict-of-term term)
    (box (field-value term)))

  (define (pict-of-stack stack)
    (box
     (apply vl-append
            (field-label "Stack")
            (map pict-of-sf (reverse stack)))))

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
  (define (pict-of-heapitem item)
    (match-define `(,this-addr ,hv) item)
    (match hv
      [`(Closure ,env ,name ,code)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-label "Closure")
                         (field "Environment @" env)
                         (field "Code" (string-of-s-exp code))))]
      [`(Environment ,bindings ,outer-addr)
       (plate (vl-append
               (field "@" this-addr)
               (white (text "Environment Frame"))
               (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key (compose symbol->string car))))))
               (field "Rest" outer-addr)))]
      [`,vec
       #:when (vector? vec)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "mvec" (apply hb-append padding (map field-value (vector->list vec))))))]
      [`(Cons ,v1 ,v2)
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2))))))]
      [else
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "cons" hv)))]))
  (define (plate p)
    (define w (pict-width p))
    (define h (pict-height p))
    (cc-superimpose
     (filled-ellipse
      (* (sqrt 2) (+ (pict-width p) 6))
      (* (sqrt 2) (+ (pict-height p) 6))
      #:draw-border? #t
      #:color "blue")
     p))

  (define (box p)
    (frame (bg "blue" (pad padding p))))
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

  (define (pict-of-focus message term ectx env)
    (bg "blue"
        (frame
         (pad padding
              (vl-append padding
                         (field "Environment @" env)
                         (ht-append padding
                                    (field message term)
                                    (field "Context" ectx)))))))

  (define (pict-of-sf sf)
    (match-define (list env ectx ann) sf)
    (bg "blue"
        (frame
         (pad padding
              (vl-append padding
                         (field "Environment @" env)
                         (ht-append padding
                                    (field "Created by" ann)
                                    (field "Context" ectx)))))))

  (define (pad n p)
    (hc-append (blank n)
               (vc-append (blank n) p (blank n))
               (blank n)))

  pict-of-state)