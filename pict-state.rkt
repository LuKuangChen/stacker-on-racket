#lang racket
(provide start-trace)
(provide pict-state)
(provide pict-terminated)
(require pict)
(require pict/color)
(require racket/gui)
(require (only-in "./utilities.rkt" string-of))

;;; save all shown picts, we may add a prev button in the future

(define-values (forward!
                backward!
                what-is-now
                add-future
                has-past?
                has-future?)
  (let ([past '()]
        [future '()]
        [now #f])
    (values
     (lambda ()
       (if (and now (pair? future))
           (begin
             (set! past (cons now past))
             (set! now (car future))
             (set! future (cdr future)))
           (error 'forward-into-nowhere)))
     (lambda ()
       (if (and now (pair? past))
           (begin
             (set! future (cons now future))
             (set! now (car past))
             (set! past (cdr past)))
           (error 'backward-into-nowhere)))
     (lambda () now)
     (lambda (item)
       (cond
         [(not now)
          (set! now item)
          now]
         [(empty? future)
          (set! future (cons item future))
          (forward!)]
         [else
          (error 'pict-manager)]))
     (lambda ()
       (pair? past))
     (lambda ()
       (pair? future)))))

(define current-go-on #f)
(define redraw! #f)
(define the-frame (new frame% [label "GUI"]))
(send the-frame create-status-line)
(define button-panel
  (new horizontal-panel%
       [parent the-frame]))
(define the-canvas
  (new canvas%
       [parent the-frame]
       [paint-callback
        (lambda (canvas dc)
          (let ([_ (let/cc k (set! redraw! (lambda () (k (void)))))])
            (let ([current-pict (what-is-now)])
              (when current-pict
                (send dc clear)
                (send canvas min-width (inexact->exact (floor (pict-width current-pict))))
                (send canvas min-height (inexact->exact (floor (pict-height current-pict))))
                (send the-last-button enable (has-past?))
                (send the-next-button enable (or (has-future?) (not terminated?)))
                (send the-frame set-status-text (if terminated? "terminated" "still running"))
                (send the-frame resize 0 0)
                (draw-pict current-pict dc 0 0)
                (when (and terminated? current-go-on)
                  (let ([go-on! current-go-on])
                    (set! current-go-on #f)
                    (go-on!)))))))]))
(define the-last-button
  (new button%
       [label "Last"]
       [parent button-panel]
       [callback
        (lambda (_button _event)
          (let ([item (backward!)])
            (redraw!)))]))
(define the-next-button
  (new button%
       [label "Next"]
       [parent button-panel]
       [callback
        (lambda (_button _event)
          (if (has-future?)
              (begin
                (forward!)
                (redraw!))
              (when current-go-on
                (let ([go-on! current-go-on])
                  (set! current-go-on #f)
                  (send the-frame refresh)
                  (go-on!)))))]))

(define (start-trace)
  (send the-frame show #t))

(define (my-show-pict p)
  (let/cc k
    (add-future p)
    (set! current-go-on (lambda () (k (void))))
    (redraw!)))

(define terminated? #f)
(define (pict-terminated printing)
  (set! terminated? #t)
  (my-show-pict (text printing)))

(define (pict-state term env ectx stack heap)
  (my-show-pict (apply pict-of-state (letrec-1->define-1 (list term env ectx stack heap)))))

(define (pict-of-state term env ectx stack heap)
  (scale
   (bg "black"
       (ht-append padding
                  (vl-append padding
                             (pict-of-stack stack)
                             (pict-of-focus term ectx env))
                  (pict-of-heap heap)))
   1.3))

(define (letrec-1->define-1 any)
  (define (rec-bind bind)
    (match-define `(,x ,e) bind)
    (match e
      [`(lambda ,arg* ,@whatever)
       `(deffun (,x ,@arg*) ,@(letrec-1->define-1 whatever))]
      [else
       `(defvar ,x ,(letrec-1->define-1 e))]))
  (define (rec-bind-1 bind)
    (match-define `(,x ,e) bind)
    (match e
      [`(lambda ,arg* ,@whatever)
       `(deffun-1 (,x ,@arg*) ,@(letrec-1->define-1 whatever))]
      [else
       `(defvar-1 ,x ,(letrec-1->define-1 e))]))
  (match any
    [`(letrec-1 ,bind* ,@prelude* ,result)
     `(,@(map rec-bind-1 bind*) ,@(map letrec-1->define-1 prelude*) ,(letrec-1->define-1 result))]
    [`(letrec ,bind* ,@prelude* ,result)
     `(,@(map rec-bind bind*) ,@(map letrec-1->define-1 prelude*) ,(letrec-1->define-1 result))]
    [else
     (if (list? any)
         (map letrec-1->define-1 any)
         any)]))

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
  (pict-of-heapitems heap)
  ;;; (define-values (envs others) (partition is-env? heap))
  ;;; (ht-append padding
  ;;;            (pict-of-envs envs)
  ;;;            (pict-of-heapitems others)
  ;;;            )
  )

(define (heapitem-interesting? item)
  (match-define `(,this-addr ,hv) item)
  (and (string? this-addr)
       (not (is-closure? hv))))
(define (is-closure? hv)
  (match hv
    [`(closure ,env ,name ,args ,def* ,body) #t]
    [else #f]))

(define (pict-of-heapitems heapitems)
  (apply vl-append padding
         (map pict-of-heapitem (filter heapitem-interesting? heapitems))))
(define (pict-of-heapitem item)
  (match-define `(,this-addr ,hv) item)
  (match hv
    [`(closure ,env ,name ,args ,def* ,body)
     (box (vl-append padding
                     (field "@" this-addr)
                     (field-label "Closure")
                     (field "Environment" env)
                     (field "Name" name)
                     (field "Definitions" (string-join (map string-of def*) " "))
                     (field-pict "Parameters" (apply hc-append padding (map field-value args)))
                     (field "Body" body)))]
    [`(Environment ,bindings ,outer-addr)
     (box (vl-append
           (field "@" this-addr)
           (white (text "Environment Frame"))
           (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                      (field-value '...)
                                      (apply vl-append padding (map pict-of-binding bindings))))
           (field "Rest" outer-addr)))]
    [else
     (box (vl-append padding
                     (field "@" this-addr)
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
  (ht-append padding (white (text name)) (bg "white" (text (format "~a" value)))))
(define (field-label name)
  (white (text name)))
(define (field-value value)
  (bg "white" (text (format "~a" value))))
(define (field-pict name p)
  (ht-append padding (white (text name)) p))

(define (bg color p)
  (cc-superimpose (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color color) p))

(define (pict-of-focus term ectx env)
  (bg "blue"
      (frame
       (pad padding
            (vl-append padding
                       (field "Environment @" env)
                       (ht-append padding
                                  (field "Computing" term)
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
