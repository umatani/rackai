#lang racket/unit
(require
 (only-in racket             match)
 (only-in "../../nondet.rkt" pure do <- :=)
 "../../signatures.rkt"
 "../../base/phases/terms.rkt")

;; Non-deterministic parsing

(import
 (only domain^    proper-stl?)
 (only syntax^    unzip strip)
 (only   bind^    resolve)
 (only     id^    core-form?))
(export parse^)

;; ----------------------------------------
;; Simple parsing of already-expanded code

;; build-var-list : Ph (Listof Id) -> (SetM (Listof Var))
(define (build-var-list ph ids Σ)
  (if (null? ids)
    (pure '())
    (do nam <- (resolve        ph (car ids) Σ)
        vs  <- (build-var-list ph (cdr ids) Σ)
        (pure (cons (Var nam) vs)))))

;; parse1 : Ph Stx Σ -> (SetM Ast)
(define ((parse1 prs1 prs*) ph stx Σ)
  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? ph 'lambda Σ)))
               (Stx (? proper-stl? stl_ids) _)
               stx_body) _)
     (do vs <- (build-var-list ph (lst->list stl_ids) Σ)
         b  <- ((prs1 prs1 prs*) ph stx_body            Σ)
         (pure (Fun vs b)))]
    
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? ph 'let Σ)))
               (Stx (? proper-stl? stl_binds) _)
               stx_body) _)
     (do (values stl_ids stl_rhs) := (unzip stl_binds)
         vs <- (build-var-list  ph (lst->list stl_ids) Σ)
         as <- ((prs* prs1 prs*) ph stl_rhs             Σ)
         b  <- ((prs1 prs1 prs*) ph stx_body            Σ)
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

    ; (#%app stx_fun stx_arg ...)
    [(Stx (Pair (? id? (? (core-form? ph '#%app Σ)))
                (Stx (Pair stx_fun stl_args) _)) _)
     (do f  <- ((prs1 prs1 prs*) ph stx_fun  Σ)
         as <- ((prs* prs1 prs*) ph stl_args Σ)
         (pure (App (gensym 'app) f as)))]

    ; (if stx stx stx)
    [(Stx (Lst (? id? (? (core-form? ph 'if Σ))) stx_test stx_then stx_else) _)
     (do c <- ((prs1 prs1 prs*) ph stx_test Σ)
         t <- ((prs1 prs1 prs*) ph stx_then Σ)
         e <- ((prs1 prs1 prs*) ph stx_else Σ)
         (pure (If (gensym 'if) c t e)))]

    ; reference
    [(? id? id)
     (do nam <- (resolve ph id Σ)
         (pure (Var nam)))]

    ; literal
    [(Stx (? Atom? a) _)
     (pure a)]))

;; parse* : Ph Stl Σ -> (SetM (Listof Ast))
(define ((parse* prs1 prs*) ph stl Σ)
  (match stl
    [(Null)
     (pure '())]

    [(Pair stx stl)
     (do ast  <- ((prs1 prs1 prs*) ph stx Σ)
         asts <- ((prs* prs1 prs*) ph stl Σ)
         (pure (cons ast asts)))]

    [(? Stx? stx)
     (do ast <- ((prs1 prs1 prs*) ph stx Σ)
         (pure (list ast)))]))

;; parse : Ph Stx Σ -> (SetM Ast)
(define parse (parse1 parse1 parse*))
