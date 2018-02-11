#lang racket

(require redex)

(define-language LF
  [t ::=
      α
      (-> t t)
      (∀ α t)]
  [e ::=
     x
     (Lam (x : t) e)
     (App e e)
     (Λ α e)
     (TyApp e t)]
  [x ::= variable-not-otherwise-mentioned])

(define-metafunction LF
  subst-x : x e e -> e
  ;; oops, no pattern binding?
  [(subst-x x_1 _ (Lam (x_1 : t_2) e_1))
   (Lam (x_1 : t_2) e_1)]
  [(subst-x x_1 e_1 (Lam (x_2 : t_2) e_2))
   (Lam (x_new : t_2)
        (subst-x x_new e_1
                 (subst-var x_2 x_new e_2)))]
  )

(define-metafunction LF
  subst-var : x any any -> any
  [(subst-var x_1 any_1 x_1) any_1]
  [(subst-var x_1 any_1 (any_2 ...))
   ((subst-var x_1 any_1 any_2) ...)]
  [(subst-var x_1 any_1 any_2) any_2])