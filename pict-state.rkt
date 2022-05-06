#lang racket
(provide pict-state)
(require pict)
(require pict/color)

(define (pict-state env ectx stack heap)
  (show-pict (pict-of-state env ectx stack heap)))

(define (pict-of-state env ectx stack heap)
  (scale
   (bg "black"
       (ht-append padding
                  (pict-of-stack stack)
                  (pict-of-heap heap)))
   1.3))

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
  (define-values (envs others) (partition is-env? heap))
  (ht-append padding
             (pict-of-envs envs)
             (pict-of-heapitems others)
             ))

(define (pict-of-heapitems heapitems)
  (apply vl-append padding (map pict-of-heapitem heapitems)))
(define (pict-of-heapitem item)
  (match-define `(,this-addr ,hv) item)
  (match hv
    [`(closure ,env ,name ,args ,body)
     (box (vl-append padding
                     (field-label "Closure")
                     (field "Address" this-addr)
                     (field "Environment" env)
                     (field "Name" name)
                     (field-pict "Parameters" (apply hc-append padding (map field-value args)))
                     (field "Body" body)))]
    [else
     (box (vl-append padding
                     (field "Address" this-addr)
                     (field "Content" hv)))])
  )

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
     (white (text "Environment Frame"))
     (field "Address" this-addr)
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
  (ht-append padding (white (text name)) (bg "white" (text (format "~a" value)))))
(define (field-label name)
  (white (text name)))
(define (field-value value)
  (bg "white" (text (format "~a" value))))
(define (field-pict name p)
  (ht-append padding (white (text name)) p))

(define (bg color p)
  (cc-superimpose (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color color) p))

(define (pict-of-sf sf)
  (match-define (list env ectx ann) sf)
  (bg "blue"
      (frame
       (pad padding
            (vl-append padding
                       (field "Environment" env)
                       (ht-append padding
                                  (field "Created by" ann)
                                  (field "Context" ectx)))))))

(define (pad n p)
  (hc-append (blank n)
             (vc-append (blank n) p (blank n))
             (blank n)))
