#lang racket
(require
 (only-in "../../mix.rkt" define-mixed-unit)
 (only-in "../../nondet.rkt" pure do <- :=)
 "../../signatures.rkt"
 "../../conc/core/terms.rkt")
(provide parse@ parser@)

;; Non-deterministic parsing

(define-unit parse@
  (import
   (only domain^    proper-stl?)
   (only syntax^    unzip strip)
   (only   menv^    init-ξ)
   (only   bind^    resolve id=? core-form?))
  (export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

  ; build-var-list : (Listof Id) -> (SetM (Listof Var))
  (define (build-var-list ids Σ)
    (if (null? ids)
      (pure '())
      (do nam <- (resolve        (car ids) Σ)
          vs  <- (build-var-list (cdr ids) Σ)
          (pure (cons (Var nam) vs)))))

  ; parse : Stx Σ -> (SetM Ast)
  (define ((parse prs prs*) stx Σ)
    (match stx
      ; (lambda (id ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'lambda Σ)))
                 (Stx stl_ids _)
                 stx_body) _)
       (do vs <- (build-var-list (lst->list stl_ids) Σ)
           b  <- ((prs prs prs*) stx_body            Σ)
           (pure (Fun vs b)))]
      
      ; (let ([id stx_rhs] ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'let Σ)))
                 (Stx (? proper-stl?  stl_binds) _)
                 stx_body) _)
       (do (values stl_ids stl_rhs) := (unzip stl_binds)
           vs <- (build-var-list  (lst->list stl_ids) Σ)
           as <- ((prs* prs prs*) stl_rhs             Σ)
           b  <- ((prs  prs prs*) stx_body            Σ)
           (pure (App (gensym 'let) (Fun vs b) as)))]

      ; (quote stx)
      [(Stx (Lst (? id? (? (core-form? 'quote Σ))) stx) _)
       (pure (let ([datum (strip stx)])
               (if (prim? datum)
                 (Prim datum stx)
                 datum)))]

      ; (syntax stx)
      [(Stx (Lst (? id? (? (core-form? 'syntax Σ))) stx) _)
       (pure stx)]

      ; (#%app stx_fun stx_arg ...)
      [(Stx (Pair (? id? (? (core-form? '#%app Σ)))
                  (Stx (Pair stx_fun stl_args) _)) _)
       (do f  <- ((prs  prs prs*) stx_fun  Σ)
           as <- ((prs* prs prs*) stl_args Σ)
           (pure (App (gensym 'app) f as)))]

      ; (if stx stx stx)
      [(Stx (Lst (? id? (? (core-form? 'if Σ))) stx_test stx_then stx_else) _)
       (do c <- ((prs prs prs*) stx_test Σ)
           t <- ((prs prs prs*) stx_then Σ)
           e <- ((prs prs prs*) stx_else Σ)
           (pure (If (gensym 'if) c t e)))]

      ; reference
      [(? id? id)
       (do nam <- (resolve id Σ)
           (pure (Var nam)))]

      ; literal
      [(Stx (? Atom? a) _)
       (pure a)]))

  ; parse* : Stl Σ -> (SetM (Listof Ast))
  (define ((parse* prs prs*) stl Σ)
    (match stl
      [(Null)
       (pure '())]

      [(Pair stx stl)
       (do ast  <- ((prs  prs prs*) stx Σ)
           asts <- ((prs* prs prs*) stl Σ)
           (pure (cons ast asts)))]

      [(? Stx? stx)
       (do ast <- ((prs prs prs*) stx Σ)
           (pure (list ast)))]))
  )

(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))
  (define parser parse))
