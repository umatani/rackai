#lang racket/unit
(require
 (only-in racket/match    match)
 (only-in "../nondet.rkt" pure do <- :=)
 "../signatures.rkt"
 "../terms.rkt")

(import
 (only domain^    proper-stl?)
 (only syntax^    unzip strip)
 (only   menv^    init-ξ)
 (only   bind^    resolve id=?))
(export parse^)

;; ----------------------------------------
;; Simple parsing of already-expanded code

; build-var-list : (Listof Id) -> (SetM (Listof Var))
(define (build-var-list #:phase [ph #f] ids Σ)
  (if (null? ids)
      (pure '())
      (do nam <- (resolve        #:phase ph (car ids) Σ)
          vs  <- (build-var-list #:phase ph (cdr ids) Σ)
          (pure (cons (Var nam) vs)))))

; parse : Ph Stx Σ -> (SetM Ast)
(define ((parse prs prs*) #:phase [ph #f] stx Σ)
  (define (core-form? nam) (λ (id) (id=? #:phase ph id nam #:ξ (init-ξ) Σ)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? 'lambda)))
               (Stx stl_ids _)
               stx_body) _)
     (do vs <- (build-var-list #:phase ph (lst->list stl_ids) Σ)
         b  <- ((prs prs prs*) #:phase ph stx_body            Σ)
         (pure (Fun vs b)))]
    
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx (Lst (? id? (? (core-form? 'let)))
               (Stx (? proper-stl?  stl_binds) _)
               stx_body) _)
     (do (values stl_ids stl_rhs) := (unzip stl_binds)
         vs <- (build-var-list  #:phase ph (lst->list stl_ids) Σ)
         as <- ((prs* prs prs*) #:phase ph stl_rhs             Σ)
         b  <- ((prs  prs prs*) #:phase ph stx_body            Σ)
         (pure (App (gensym 'let) (Fun vs b) as)))]

    ; (quote stx)
    [(Stx (Lst (? id? (? (core-form? 'quote))) stx) _)
     (pure (let ([datum (strip stx)])
             (if (prim? datum)
                 (Prim datum stx)
                 datum)))]

    ; (syntax stx)
    [(Stx (Lst (? id? (? (core-form? 'syntax))) stx) _)
     (pure stx)]

    ; (#%app stx_fun stx_arg ...) Note that it is non-proper (i.e. pair of stxs)
    [(Stx (Pair (? id? (? (core-form? '#%app)))
                (Stx (Pair stx_fun stl_args) _)) _)
     (do f  <- ((prs  prs prs*) #:phase ph stx_fun  Σ)
         as <- ((prs* prs prs*) #:phase ph stl_args Σ)
         (pure (App (gensym 'app) f as)))]

    ; (if stx stx stx)
    [(Stx (Lst (? id? (? (core-form? 'if))) stx_test stx_then stx_else) _)
     (do c <- ((prs prs prs*) #:phase ph stx_test Σ)
         t <- ((prs prs prs*) #:phase ph stx_then Σ)
         e <- ((prs prs prs*) #:phase ph stx_else Σ)
         (pure (If (gensym 'if) c t e)))]

    ; reference
    [(? id? id)
     (do nam <- (resolve #:phase ph id Σ)
         (pure (Var nam)))]

    ; literal
    [(Stx (? Atom? a) _)
     (pure a)]))

; parse* : Ph Stl Σ -> (SetM (Listof Ast))
(define ((parse* prs prs*) #:phase [ph #f] stl Σ)
  (match stl
    [(Null)
     (pure '())]

    [(Pair stx stl)
     (do ast  <- ((prs  prs prs*) #:phase ph stx Σ)
         asts <- ((prs* prs prs*) #:phase ph stl Σ)
         (pure (cons ast asts)))]

    [(? Stx? stx)
     (do ast <- ((prs prs prs*) #:phase ph stx Σ)
         (pure (list ast)))]))
