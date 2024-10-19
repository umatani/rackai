#lang racket/unit
(require
 (only-in racket/match       match)
 (only-in "../../nondet.rkt" pure do <- :=)
 "../../signatures.rkt"
 "../../base/core/terms.rkt")

;; Non-deterministic parsing

(import
 (only domain^    proper-stl?)
 (only syntax^    unzip strip)
 (only   menv^    init-ξ)
 (only   bind^    resolve)
 (only     id^    core-form?))
(export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

;; build-var-list : (Listof Id) -> (SetM (Listof Var))
(define (build-var-list ids Σ)
  (if (null? ids)
    (pure '())
    (do nam <- (resolve        (car ids) Σ)
        vs  <- (build-var-list (cdr ids) Σ)
        (pure (cons (Var nam) vs)))))

;; parse1 : Stx Σ -> (SetM Ast)
(define ((parse1 prs1 prs*) stx Σ)
  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? 'lambda Σ)))
               (Stx (? proper-stl? stl_ids) _)
               stx_body) _)
     (do vs <- (build-var-list (lst->list stl_ids) Σ)
         b  <- ((prs1 prs1 prs*) stx_body            Σ)
         (pure (Fun vs b)))]
    
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? 'let Σ)))
               (Stx (? proper-stl? stl_binds) _)
               stx_body) _)
     (do (values stl_ids stl_rhs) := (unzip stl_binds)
         vs <- (build-var-list  (lst->list stl_ids) Σ)
         as <- ((prs* prs1 prs*) stl_rhs             Σ)
         b  <- ((prs1 prs1 prs*) stx_body            Σ)
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
     (do f  <- ((prs1 prs1 prs*) stx_fun  Σ)
         as <- ((prs* prs1 prs*) stl_args Σ)
         (pure (App (gensym 'app) f as)))]

    ; (if stx stx stx)
    [(Stx (Lst (? id? (? (core-form? 'if Σ))) stx_test stx_then stx_else) _)
     (do c <- ((prs1 prs1 prs*) stx_test Σ)
         t <- ((prs1 prs1 prs*) stx_then Σ)
         e <- ((prs1 prs1 prs*) stx_else Σ)
         (pure (If (gensym 'if) c t e)))]

    ; reference
    [(? id? id)
     (do nam <- (resolve id Σ)
         (pure (Var nam)))]

    ; literal
    [(Stx (? Atom? a) _)
     (pure a)]))

;; parse* : Stl Σ -> (SetM (Listof Ast))
(define ((parse* prs1 prs*) stl Σ)
  (match stl
    [(Null)
     (pure '())]

    [(Pair stx stl)
     (do ast  <- ((prs1 prs1 prs*) stx Σ)
         asts <- ((prs* prs1 prs*) stl Σ)
         (pure (cons ast asts)))]

    [(? Stx? stx)
     (do ast <- ((prs1 prs1 prs*) stx Σ)
         (pure (list ast)))]))

; parse : Stx Σ -> (SetM Ast)
(define parse (parse1 parse1 parse*))
