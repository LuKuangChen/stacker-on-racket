#lang racket
(provide string-of-s-exp block)
(require pprint)

(define block (gensym 'block))

(define (string-of-s-exp e)
  (pretty-format (doc-of-s-exp e)))
(define (doc-of-s-exp e)
  (match e
    [`(,maybe-block ,@body)
     #:when (eqv? maybe-block block)
     (apply v-append (map doc-of-s-exp body))]
    #;
    [`(defvar ,x (lambda (,@arg*) ,body))
     (nest 2 (v-append (h-append (text "(defvar ") (doc-of-s-exp x))
                        (h-append (doc-of-s-exp `(lambda (,@arg*) ,body)) (text ")"))))]
    #;
    [`(defvar ,x ,e)
     (nest 2 (vs-append (h-append (text "(defvar ") (doc-of-s-exp x))
                        (h-append (doc-of-s-exp e) (text ")"))))]
    #;
    [`(begin ,@e*)
     (nest 2 (v-append (text "(begin ")
                (h-append (v-concat (map doc-of-s-exp e)) (text ")"))))]
    #;
    [`(lambda (,@arg*) ,body)
     (align (nest 2 (v-append (h-append (text "(lambda ") (doc-of-s-exp arg*))
                       (h-append (doc-of-s-exp body)
                                 (text ")")))))]
    [else
     (if (list? e)
         (h-append (text "(") (apply hs-append (map doc-of-s-exp e)) (text ")"))
         (text (format "~a" e)))]))