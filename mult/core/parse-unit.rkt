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

;; ----------------------------------------
;; Simple parsing of already-expanded code

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
(define (parse1 stx Σ)
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
            (do f  <- (parse1 stx_fun  Σ)
                as <- (parse* stl_args Σ)
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
                b  <- (parse1 stx_body Σ)
                (pure (Fun vs b)))]
           ; (let ([id stx_rhs] ...) stx_body)
           [(let)
            (match-define (Lst (Stx (? proper-stl? stl_binds) _)
                               stx_body) stl)
            (do (values stl_ids stl_rhs) := (unzip stl_binds)
                vs <- (build-vars (lst→list stl_ids) Σ)
                as <- (parse* stl_rhs  Σ)
                b  <- (parse1 stx_body Σ)
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
            (do c <- (parse1 stx_test Σ)
                t <- (parse1 stx_then Σ)
                e <- (parse1 stx_else Σ)
                (pure (If (gensym 'if) c t e)))]
           [else (error 'parse "unknown op: ~a\n" op)]))]
    [_ (error 'parse "unknown form: ~a\n" (lst→list/recur (stx→datum stx)))]))

;; parse* : Stl Σ → (SetM (Listof Ast))
(define (parse* stl Σ)
  (match stl
    [(Null)
     (pure '())]

    [(Pair stx stl)
     (do ast  <- (parse1 stx Σ)
         asts <- (parse* stl Σ)
         (pure (cons ast asts)))]

    [(? Stx? stx)
     (do ast <- (parse1 stx Σ)
         (pure (list ast)))]))

; parse : Stx Σ → (SetM Ast)
(define parse parse1)
