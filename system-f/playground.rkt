#lang racket

(require redex)

(define-language LF
  [t ::= 
     *
     (-> t t)]
  [e :=
     x
     (App e e)
     (Lam x t e)]
  
  [x variable-not-otherwise-mentioned]
  [α variable-not-otherwise-mentioned])

