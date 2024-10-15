#lang racket
(require
 (only-in "../../mix.rkt"    define-mixed-unit)
 (only-in "../../nondet.rkt" pure do <- :=)
 "../../signatures.rkt"
 "../../conc/phases/terms.rkt")
(provide parse@ parser@)

;; Non-deterministic parsing

(define-unit parse@
  (import
   (only domain^    proper-stl?)
   (only syntax^    unzip strip)
   (only   bind^    resolve core-form?))
  (export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

  ; build-var-list : Ph (Listof Id) -> (SetM (Listof Var))
  (define (build-var-list ph ids Σ)
    (if (null? ids)
      (pure '())
      (do nam <- (resolve        ph (car ids) Σ)
          vs  <- (build-var-list ph (cdr ids) Σ)
          (pure (cons (Var nam) vs)))))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define ((parse prs prs*) ph stx Σ)
    (match stx
      ; (lambda (id ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? ph 'lambda Σ)))
                 (Stx stl_ids _)
                 stx_body) _)
       (do vs <- (build-var-list ph (lst->list stl_ids) Σ)
           b  <- ((prs prs prs*) ph stx_body            Σ)
           (pure (Fun vs b)))]
      
      ; (let ([id stx_rhs] ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? ph 'let Σ)))
                 (Stx (? proper-stl?  stl_binds) _)
                 stx_body) _)
       (do (values stl_ids stl_rhs) := (unzip stl_binds)
           vs <- (build-var-list  ph (lst->list stl_ids) Σ)
           as <- ((prs* prs prs*) ph stl_rhs             Σ)
           b  <- ((prs  prs prs*) ph stx_body            Σ)
           (pure (App (gensym 'let) (Fun vs b) as)))]

      ; (quote stx)
      [(Stx (Lst (? id? (? (core-form? ph 'quote Σ))) stx) _)
       (pure (let ([datum (strip stx)])
               (if (prim? datum)
                 (Prim datum stx)
                 datum)))]

      ; (syntax stx)
      [(Stx (Lst (? id? (? (core-form? ph 'syntax Σ))) stx) _)
       (pure stx)]

      ; (#%app stx_fun stx_arg ...) Note that it is non-proper (i.e. pair of stxs)
      [(Stx (Pair (? id? (? (core-form? ph '#%app Σ)))
                  (Stx (Pair stx_fun stl_args) _)) _)
       (do f  <- ((prs  prs prs*) ph stx_fun  Σ)
           as <- ((prs* prs prs*) ph stl_args Σ)
           (pure (App (gensym 'app) f as)))]

      ; (if stx stx stx)
      [(Stx (Lst (? id? (? (core-form? ph 'if Σ))) stx_test stx_then stx_else) _)
       (do c <- ((prs prs prs*) ph stx_test Σ)
           t <- ((prs prs prs*) ph stx_then Σ)
           e <- ((prs prs prs*) ph stx_else Σ)
           (pure (If (gensym 'if) c t e)))]

      ; reference
      [(? id? id)
       (do nam <- (resolve ph id Σ)
           (pure (Var nam)))]

      ; literal
      [(Stx (? Atom? a) _)
       (pure a)]))

  ; parse* : Ph Stl Σ -> (SetM (Listof Ast))
  (define ((parse* prs prs*) ph stl Σ)
    (match stl
      [(Null)
       (pure '())]

      [(Pair stx stl)
       (do ast  <- ((prs  prs prs*) ph stx Σ)
           asts <- ((prs* prs prs*) ph stl Σ)
           (pure (cons ast asts)))]

      [(? Stx? stx)
       (do ast <- ((prs prs prs*) ph stx Σ)
           (pure (list ast)))]))
  )


(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))

  ; parser : Stx Σ → (SetM Ast)
  (define (parser stx Σ) (parse 0 stx Σ)))
