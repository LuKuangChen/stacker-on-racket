#lang racket
(provide pict-of-state)
(require pict)
(require (rename-in pict [text pict-text]))
(require racket/draw)

;;; This color palette has been checked with
;;;   https://color.adobe.com/zh/create/color-accessibility
;; dark and light blue (#0000C8 & #0000FF)
(define color-A-D (make-object color% 0 0 200))
(define color-A-L (make-object color% 0 0 255))
;; dark and light yellow (#FFBB00 & #FFDF40)
(define color-B-D (make-object color% 255 187 0))
(define color-B-L (make-object color% 255 223 64))
;; red (#FF7D6C)
(define color-C (make-object color% 255 127 121))
;; green (#C1E197)
(define color-D (make-object color% 193 225 151))
;; black and white
(define color-black (make-object color% 0 0 0))
(define color-white (make-object color% 255 255 255))

;; These color palettes for texts have been checked with
;;   https://webaim.org/resources/contrastchecker/
(struct text-palette
  (text background))
(define tp-A-D (text-palette color-white color-A-D))
(define tp-A-L (text-palette color-white color-A-L))
(define tp-B-D (text-palette color-black color-B-D))
(define tp-B-L (text-palette color-black color-B-L))
(define tp-C (text-palette color-black color-C))
(define tp-D (text-palette color-black color-D))
(define tp-white (text-palette color-black color-white))
(define tp-black (text-palette color-white color-black))

(define tp-stack tp-black)
(define tp-stack-frame tp-B-D)
(define tp-calling tp-B-D)
(define tp-called tp-A-D)
(define tp-returning tp-B-L)
(define tp-returned tp-A-L)
(define tp-terminated tp-black)
(define tp-errored tp-C)

(define current-text-palette (make-parameter tp-white))
(define (current-text-color)
  (text-palette-text (current-text-palette)))
(define (current-background-color)
  (text-palette-background (current-text-palette)))

(define (text s)
  (pre-text s 'modern))
(define (pre-text s font-family)
  (define style
    (cons (current-text-color) font-family))
  (if (equal? s "")
      (pict-text " " style)
      (apply vl-append
         (map
           (lambda (s)
             (pict-text s style)) 
           (string-split s "\n")))))

(define (pict-of-state hide-closure? hide-env-label?)
  (define ((pict-of-focus heap) focus)
    (match focus
      [`("calling" ,app ,env ,ectx)
       (parameterize ([current-text-palette tp-calling])
         (plate (vl-append padding
                         (field "Calling" app)
                         (field "in context" ectx)
                         (field "in environment @" env))))]
      [`("called" ,body ,env)
       (parameterize ([current-text-palette tp-called])
       (plate (vl-append padding
                         (field-label "Evaluating the function body")
                         (field-value body)
                         (field "in environment @" env))))]
      [`("returned" ,v ,env ,ectx)
       (parameterize ([current-text-palette tp-returned])
       (plate (vl-append padding
                         (field "Returned" v)
                         (field "to contect" ectx)
                         (field "in environment @" env))))]
      [`("returning" ,v)
       (parameterize ([current-text-palette tp-returning])
       (plate (vl-append padding
                         (field "Returning" v))))]
      [`("terminated" ,v*)
       (parameterize ([current-text-palette tp-terminated])
       (plate (vl-append padding
                         (field-label "Terminated")
                         (field-value (string-join v* "\n")))))]
      [`("errored")
       (parameterize ([current-text-palette tp-errored])
       (plate (vl-append padding
                         (field-label "Errored"))))]))

  (define (main-pict stack focus heap)
    (bg (ht-append padding
                   (vl-append padding
                              ((pict-of-stack heap) stack)
                              ((pict-of-focus heap) focus))
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
        [`("returning" ,v ,stack ,heap)
         (main-pict stack `("returning" ,v) heap)]
        [`("terminated" ,v* ,heap)
         (main-pict empty `("terminated" ,v*) heap)]
        [`("errored" ,heap)
         (main-pict empty `("errored") heap)]))
    (define dim (max (pict-width p) (pict-height p)))
    (scale p (min 1.2 (/ 700 (pict-height p)) (/ 1200 (pict-width p)))))

  (define ((pict-of-stack heap) stack)
    (parameterize ([current-text-palette tp-stack])
    (box
     (apply vl-append
            (field-label "Stack")
            (map (pict-of-sf heap) (reverse stack))))))

  (define (is-env? heapitem)
    (match-define (list addr hv) heapitem)
    (match hv
      [`("env" ,@_)
       #t]
      [else
       #f]))

  (define (pict-of-heap heap)
    (pict-of-heapitem* heap))

  (define (heapitem-interesting? item)
    (match-define `(,this-addr ,hv) item)
    (and (string? this-addr) ;; string addresses means the address is not primitive (symbol address)
         (if hide-closure?
             (not (is-closure? hv))
             #t)))
  (define (is-closure? hv)
    (match hv
      [`("fun" ,@_) #t]
      [else #f]))
  (define (pict-of-heapitem* heapitems)
    (let-values ([(envs others) (partition is-env?
                                           (filter heapitem-interesting? heapitems))])
      (ht-append
       padding
       (apply vl-append padding
              (map pict-of-heapitem envs))
       (apply vl-append padding
              (map pict-of-heapitem others)))))
  (define (pict-of-heapitem item)
    (match-define `(,this-addr ,hv) item)
    (match hv
      [`("env" ,env ,bindings)
       (parameterize ([current-text-palette tp-D])
       (plate (vl-append
               (field "@" this-addr)
               (if hide-env-label?
                   (blank)
                   (field-label "Environment Frame"))
               (field-pict "Bindings" (if (equal? this-addr '|@base-env|)
                                          (field-value '...)
                                          (apply vl-append padding
                                                 (map pict-of-binding
                                                      (sort bindings string<=? #:key first)))))
               (field "Rest @" env))
       ))]
      [`("fun" ,env ,code)
       (parameterize ([current-text-palette tp-black])
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field "Environment @" env)
                         (field "Code" code))
              ))]
      [`("vec" ,vec)
       (parameterize ([current-text-palette tp-black])
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "mvec" (apply hb-append padding (map field-value vec))))
       ))]
      [`("cons" ,v1 ,v2)
       (parameterize ([current-text-palette tp-black])
       (plate (vl-append padding
                         (field "@" this-addr)
                         (field-pict "cons" (apply hb-append padding (map field-value (list v1 v2)))))
              ))]))
  (define (plate p)
    (define w (pict-width p))
    (define h (pict-height p))
    (define r 10)
    (cc-superimpose
     (filled-rounded-rectangle
       (+ (pict-width p) (* r 2))
       (+ (pict-height p) (* r 2))
       r
       #:color (current-background-color))
     p))

  (define (box p)
    (frame (bg (pad padding p))))
  (define (pict-of-binding binding)
    (match-define (list x v) binding)
    (ht-append padding
               (field-value x)
               (field-label "↦")
               (field-value v)))

  (define padding 5)

  (define (field-label name)
    (pre-text name 'system))
  (define (field-value value)
    (parameterize ([current-text-palette tp-white])
      (bg (text value))))
  (define (field-pict name p)
    (ht-append padding (field-label name) p))
  (define (field name value)
    (field-pict name (field-value value)))

  (define (bg p)
    (cc-superimpose
      (filled-rectangle
        (pict-width p)
        (pict-height p)
        #:draw-border? #f
        #:color (current-background-color))
      p))

  (define ((pict-of-sf heap) sf)
    (match-define (list env ectx ann) sf)
    (parameterize ([current-text-palette tp-stack-frame])
      (bg (frame
         (pad padding
              (vl-append padding
                         (field-label "Waiting for a value")
                         (field "in context" ectx)
                         (field "in environment @" env)))))))

  (define (pad n p)
    (hc-append (blank n)
               (vc-append (blank n) p (blank n))
               (blank n)))

  pict-of-state)