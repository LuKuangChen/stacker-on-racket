#lang plait
(define-type-alias Id Symbol)

(define-type Constant
  (c-void)
  (c-str [it : String])
  (c-num [it : Number])
  (c-bool [it : Boolean])
  (c-char [it : Char])
  (c-vec [it : (Listof Constant)])
  (c-list [it : (Listof Constant)]))
(define-type-alias Program ((Listof Def) * (Listof Expr)))
(define (program d* e*) (pair d* e*))
(define-type Def
  [d-fun [fun : Id] [arg* : (Listof Id)]
         [def* : (Listof Def)]
         [prelude* : (Listof Expr)]
         [result : Expr]]
  [d-var [var : Id] [val : Expr]])
(define-type Expr
  (e-con [c : Constant])
  (e-var [x : Id])
  (e-fun [arg* : (Listof Id)]
         [def* : (Listof Def)]
         [prelude* : (Listof Expr)]
         [result : Expr])
  (e-app [fun : Expr] [arg* : (Listof Expr)])
  (e-let [bind* : (Listof Bind)]
         [def* : (Listof Def)]
         [prelude* : (Listof Expr)]
         [result : Expr])
  (e-let* [bind* : (Listof Bind)]
          [def* : (Listof Def)]
          [prelude* : (Listof Expr)]
          [result : Expr])
  (e-letrec [bind* : (Listof Bind)]
            [def* : (Listof Def)]
            [prelude* : (Listof Expr)]
            [result : Expr])
  (e-set! [var : Id] [val : Expr])
  (e-begin [prelude* : (Listof Expr)] [result : Expr])
  (e-if [cnd : Expr] [thn : Expr] [els : Expr])
  (e-cond [cnd-thn* : (Listof (Expr * Expr))] [els : (Optionof Expr)]))
(define (bind x e) (values x e))
(define-type-alias Bind (Id * Expr))
(define (var-of-bind bind) (fst bind))
(define (val-of-bind bind) (snd bind))
(define-type Obs
  (o-void)
  (o-con [it : Constant])
  (o-vec [it : (Vectorof Obs)])
  (o-list [it : (Listof Obs)])
  (o-fun [it : (Optionof String)])
  (o-rec [id : Number] [content : Obs])
  (o-var [id : Number])
  (o-exn [it : String]))
