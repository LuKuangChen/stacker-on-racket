#lang racket
(provide pict-loop)
(require pict)
(require racket/gui)
(require web-server/servlet
         web-server/servlet-env)

(require pict
         net/base64
         file/convertible)

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
  ; render: request -> doesn't return
  ; Produces an HTML page of the content of the BLOG.

  (define (prevable?)
    (has-past?))

  (define (nextable?)
    (or (has-future?)
        (not (terminate? state))))

  (define (render request)
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (title "Stacker"))
              (body
               (div ((style "display: flex; flex-direction: row;"))
                (form ((action ,(embed/url handle-prev)))
                      (input ((type "submit")
                              (value "Prev")
                              ,@(if (prevable?) '() '((disabled ""))))))
                (form ((action ,(embed/url handle-next)))
                      (input ((type "submit")
                              (value "Next")
                              ,@(if (nextable?) '() '((disabled "")))))))
               ,(display-state)
               ;;;  (pre ,(format "~a" (prevable?)))
               ;;;  (pre ,(format "~a" (nextable?)))
               ;;;  (pre ,(format "~a" (has-future?)))
               ;;;  (pre ,(format "~a" (terminate? state)))
               ;;;  (pre ,(format "~a" state))
               ))))

    (define (handle-prev request)
      (backward!)
      (render request))

    (define (handle-next request)
      (if (has-future?)
          (forward!)
          (unless (terminate? state)
            (step!)))
      (render request))

    (send/suspend/dispatch response-generator))

  (define (pict->data-uri pict)
    (format "data:image/svg+xml;base64,~a"
            (base64-encode (convert pict 'svg-bytes))))

  (define (display-state)
    `(img ([src ,(pict->data-uri (what-is-now))]
           [width "100%"]
           [height "100%"])))

  (define (start request)
    (render request))

  (define (step!)
    (set! state (forward state))
    (add-future (pict-of-state state)))
  (add-future (pict-of-state state))

  (serve/servlet start))
