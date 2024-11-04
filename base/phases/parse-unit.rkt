#lang racket/unit
(require
 (only-in racket/match       match match-define)
 (only-in "../../nondet.rkt" pure)
 (only-in "../../syntax.rkt" unzip strip)
 "../../signatures.rkt"
 "terms.rkt")

(import
 (only syntax^    proper-stl?)
 (only   bind^    resolve))
(export parse^)

;; ----------------------------------------
;; Simple parsing of already-expanded code

;; parse1 : Ph Stx Σ → Ast
(define (parse1 ph stx Σ)
  (match stx
    ; reference
    [(? id? id) (Var (resolve ph id Σ))]

    ; literal
    [(Stx (? Atom? a) _) a]

    [(Stx (Pair (? id? id) (Stx stl _)) _)
     (case (resolve ph id Σ)
       ; (#%app stx_fun stx_arg ...)
       [(#%app)
        (match-define (Lst stx_fun . stl_args) stl)
        (App (gensym 'app)
             (parse1 ph stx_fun Σ)
             (parse* ph stl_args Σ))]
       [else (error 'parse "unknown op: ~a\n" (resolve ph id Σ))])]

    [(Stx (Lst (? id? id) . stl) _)
     (case (resolve ph id Σ)
       ; (lambda (id ...) stx_body)
       [(lambda)
        (match-define (Lst(Stx (? proper-stl? stl_ids) _)
                          stx_body) stl)
        (Fun (map (λ (id) (Var (resolve ph id Σ)))
                  (lst→list stl_ids))
             (parse1 ph stx_body Σ))]
       ; (let ([id stx_rhs] ...) stx_body)
       [(let)
        (match-define (Lst (Stx (? proper-stl? stl_binds) _)
                           stx_body) stl)
        (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
          (App (gensym 'let)
               (Fun (map (λ (id) (Var (resolve ph id Σ)))
                         (lst→list stl_ids))
                    (parse1 ph stx_body Σ))
               (parse* ph stl_rhs Σ)))]
       ; (quote stx)
       [(quote)
        (match-define (Lst stx) stl)
        (let ([datum (strip stx)])
          (if (prim? datum)
            (Prim datum stx) ;; stx is used for alloc-box, alloc-def-ξ
            datum))]
       ; (syntax stx)
       [(syntax)
        (match-define (Lst stx) stl)
        stx]
       ; (if stx stx stx)
       [(if)
        (match-define (Lst stx_test stx_then stx_else) stl)
        (If (gensym 'if)
            (parse1 ph stx_test Σ)
            (parse1 ph stx_then Σ)
            (parse1 ph stx_else Σ))]
       [else (error 'parse "unknown op: ~a\n" (resolve ph id Σ))])]))

;; parse* : Ph Stl Σ → (Listof Ast)
(define (parse* ph stl Σ)
  (match stl
    [(Null) '()]
    [(Pair stx stl)
     (cons (parse1 ph stx Σ) (parse* ph stl Σ))]
    [(? Stx? stx)
     (list (parse1 ph stx Σ))]))

;; parse : Ph Stx Σ → (SetM Ast)
(define (parse ph stx Σ)
  (pure (parse1 ph stx Σ)))
