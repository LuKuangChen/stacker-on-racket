#lang racket
(provide pict-loop)
(require pict)
(require racket/gui)

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



(define (pict-loop state terminate? forward pict-of-state)
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
            (let ([current-pict (what-is-now)])
              (when current-pict
                (send dc clear)
                (send canvas min-width (inexact->exact (floor (pict-width current-pict))))
                (send canvas min-height (inexact->exact (floor (pict-height current-pict))))
                (send the-last-button enable (has-past?))
                (send the-next-button enable (or (has-future?) (not (terminate? state))))
                (send the-frame set-status-text (if (terminate? state) "terminated" "still running"))
                (send the-frame resize 10 10)
                (draw-pict current-pict dc 0 0))))]))
  (define the-last-button
    (new button%
         [label "Last"]
         [parent button-panel]
         [callback
          (lambda (_button _event)
            (let ([item (backward!)])
              (send the-frame refresh)))]))
  (define the-next-button
    (new button%
         [label "Next"]
         [parent button-panel]
         [callback
          (lambda (_button _event)
            (if (has-future?)
                (forward!)
                (unless (terminate? state)
                  (step!)))
            (send the-frame refresh))]))
  (define (step!)
    (set! state (forward state))
    (add-future (pict-of-state state)))
  (add-future (pict-of-state state))
  (send the-frame show #t))