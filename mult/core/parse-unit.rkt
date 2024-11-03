#lang racket/unit
(require
 (only-in racket/match       match match-define)
 (only-in "../../nondet.rkt" do := <- pure)
 (only-in "../../syntax.rkt" stx→datum)
 "../../signatures.rkt"
 "../../base/core/terms.rkt")

;; Non-deterministic parsing

(import
 (only domain^    proper-stl?)
 (only syntax^    unzip strip)
 (only   bind^    resolve))
(export parse^)

;; -----------------------------------------------------
;; Parsing of already-expanded code
;;   rewrite with open recursions to enable hooks in abs

;; build-vars : (Listof Id) → (SetM (Listof Var))
(define (build-vars ids Σ)
  (match ids
    ['()
     (pure '())]
    [(cons id ids)
     (do nam <- (resolve    id Σ)
         vs  <- (build-vars ids Σ)
         (pure (cons (Var nam) vs)))]))

;; parse1 : Stx Σ → (SetM Ast)
(define ((parse1 prs1 prs*) stx Σ)
  (match stx
    ; reference
    [(? id? id)
     (do nam <- (resolve id Σ)
         (pure (Var nam)))]

    ; literal
    [(Stx (? Atom? a) _)
     (pure a)]

    [(Stx (Pair (? id? id) (Stx stl _)) _)
     (do op <- (resolve id Σ)
         (case op
           ; (#%app stx_fun stx_arg ...)
           [(#%app)
            (match-define (Lst stx_fun . stl_args) stl)
            (do f  <- ((prs1 prs1 prs*) stx_fun  Σ)
                as <- ((prs* prs1 prs*) stl_args Σ)
                (pure (App (gensym 'app) f as)))]
           [else (error 'parse "unknown op: ~a\n" op)]))]

    [(Stx (Lst (? id? id) . stl) _)
     (do op <- (resolve id Σ)
         (case op
           ; (lambda (id ...) stx_body)
           [(lambda)
            (match-define (Lst(Stx (? proper-stl? stl_ids) _)
                              stx_body) stl)
            (do vs <- (build-vars (lst→list stl_ids) Σ)
                b  <- ((prs1 prs1 prs*) stx_body     Σ)
                (pure (Fun vs b)))]
           ; (let ([id stx_rhs] ...) stx_body)
           [(let)
            (match-define (Lst (Stx (? proper-stl? stl_binds) _)
                               stx_body) stl)
            (do (values stl_ids stl_rhs) := (unzip stl_binds)
                vs <- (build-vars (lst→list stl_ids) Σ)
                as <- ((prs* prs1 prs*) stl_rhs      Σ)
                b  <- ((prs1 prs1 prs*) stx_body     Σ)
                (pure (App (gensym 'let) (Fun vs b) as)))]
           ; (quote stx)
           [(quote)
            (match-define (Lst stx) stl)
            (pure (let ([datum (strip stx)])
                    (if (prim? datum)
                      (Prim datum stx)
                      datum)))]
           ; (syntax stx)
           [(syntax)
            (match-define (Lst stx) stl)
            (pure stx)]
           ; (if stx stx stx)
           [(if)
            (match-define (Lst stx_test stx_then stx_else) stl)
            (do c <- ((prs1 prs1 prs*) stx_test Σ)
                t <- ((prs1 prs1 prs*) stx_then Σ)
                e <- ((prs1 prs1 prs*) stx_else Σ)
                (pure (If (gensym 'if) c t e)))]
           [else (error 'parse "unknown op: ~a\n" op)]))]
    [_ (error 'parse "unknown form: ~a\n" (lst→list/recur (stx→datum stx)))]))




;; parse* : Stl Σ → (SetM (Listof Ast))
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

; parse : Stx Σ → (SetM Ast)
(define parse (parse1 parse1 parse*))
