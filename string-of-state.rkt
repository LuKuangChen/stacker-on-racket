#lang racket
(provide string-of-s-exp block)
(require pprint)

(define block (gensym 'block))

(define (string-of-s-exp e)
  (pretty-format (doc-of-s-exp e) 17))
(define (doc-of-s-exp e)
  (match e
    [`(,maybe-block ,@body)
     #:when (eqv? maybe-block block)
     (apply v-append (map doc-of-s-exp body))]
    [`(,defvar/set! ,x ,e)
     #:when (memv defvar/set! '(defvar defvar-1 set!))
     (align
      (nest 2 (vs-append (h-append (text (format "(~a " defvar/set!)) (doc-of-s-exp x))
                         (h-append (doc-of-s-exp e) (text ")")))))]
    [`(,deffun? ,head ,@body)
     #:when (memv deffun? '(deffun deffun-1))
     (align
      (nest 2 (vs-append (h-append (text (format "(~a " deffun?)) (doc-of-s-exp head))
                         (h-append (vs-concat (map doc-of-s-exp body)) (text ")")))))]
    [`(begin ,@e*)
     (align
      (nest 2 (v-append (text "(begin ")
                        (h-append (v-concat (map doc-of-s-exp e*)) (text ")")))))]
    [`(lambda (,@arg*) ,body)
     (align (nest 2 (v-append (h-append (text "(lambda ") (doc-of-s-exp arg*))
                              (h-append (doc-of-s-exp body)
                                        (text ")")))))]
    [`(if ,cnd ,thn ,els)
     (align (h-append (text "(if ")
                      (align (v-append (doc-of-s-exp cnd)
                                       (doc-of-s-exp thn)
                                       (doc-of-s-exp els)))
                      (text ")")))]
    [else
     (if (list? e)
         (h-append (text "(") (apply hs-append (map doc-of-s-exp e)) (text ")"))
         (text (format "~a" e)))]))