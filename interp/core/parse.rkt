#lang racket
(require "struct.rkt"
         (only-in "syntax.rkt" unzip strip resolve))
(provide (all-defined-out))

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

;(: parse : Stx Σ -> Ast)
(define (parse stx Σ)
  (define (id=? nam)
    (λ (id) (eq? (resolve id Σ) nam)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'lambda)))
               ,(GenStx stl_ids _) ,stx_body) _)
     (Fun (map (λ (id) (Var (resolve id Σ)))
               stl_ids)
          (parse stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'let)))
               ,(GenStx (? ProperStl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (Fun (map (λ (id) (Var (resolve id Σ)))
                      stl_ids)
                 (parse stx_body Σ))
            (map (λ (stx_rhs) (parse stx_rhs Σ))
                 stl_rhs)))]
    ; (quote stx)
    [(GenStx `(,(? Id? (? (id=? 'quote))) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(GenStx `(,(? Id? (? (id=? 'syntax))) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(GenStx (cons (? Id? (? (id=? '#%app)))
                   (GenStx (cons stx_fun stl_args) _)) _)
     (App (parse stx_fun Σ) (parse* stl_args Σ))]
    ; (if stx stx stx)
    [(GenStx `(,(? Id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (If (parse stx_test Σ) (parse stx_then Σ) (parse stx_else Σ))]
    ; reference
    [(? Id? id) (Var (resolve id Σ))]
    ; literal
    [(GenStx (? Atom? atom) _) atom]))

;(: parse* : Stl Σ -> (Listof Ast))
(define (parse* stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse stx Σ) (parse* stl Σ))]
    [stx (list (parse stx Σ))]))
