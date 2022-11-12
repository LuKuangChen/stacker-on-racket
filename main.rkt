#lang racket
;;; (require "interp.rkt")
;;; (require pict
;;;          net/base64
;;;          file/convertible)

;;; ; start: request -> doesn't return
;;; ; Consumes a request and produces a page that displays
;;; ; all of the web content.
;;; (define (start request)
;;;   (interp '(((lambda (x) (lambda (y) x)) 2) 3))
;;;   (render request))

;;; ; render: request -> doesn't return
;;; ; Produces an HTML page of the content of the BLOG.
;;; (define (render request)
;;;   (define (response-generator embed/url)
;;;     (response/xexpr
;;;      `(html (head (title "Status"))
;;;             (body
;;;              (h1 "Status")
;;;              ,(display-state)
;;;              (form ((action ,(embed/url handle-prev)))
;;;                    (input ((type "Prev"))))
;;;                    (form ((action ,(embed/url handle-next)))
;;;                    (input ((type "Next"))))))))

;;;   (define (handle-prev request)
;;;     (resume!)
;;;     (render request))

;;;   (define (handle-next request)
;;;     (resume!)
;;;     (render request))

;;;   (send/suspend/dispatch response-generator))

;;; (define (visualization-of-current-state)
;;;   (string-join
;;;    (list
;;;     (format "stack: ~a" (current-stack))
;;;     (format "heap: ~a" (current-heap))
;;;     (format "cont: ~a" (current-cont))
;;;     (format "env: ~a" (current-env)))
;;;    "\n"))

;;; ;; pict->data-uri : Pict -> String
;;; (define (pict->data-uri pict)
;;;   (format "data:image/png;base64,~a"
;;;           (base64-encode (convert pict 'png-bytes))))

;;; ; display-state: -> xexpr
;;; ; Consumes a blog, produces an xexpr fragment
;;; ; of all its posts.
;;; (define (display-state)
;;;   `(img ([src ,(pict->data-uri (text (visualization-of-current-state)))])))