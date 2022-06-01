#lang racket
(require (only-in "../core/syntax.rkt" unzip strip)
         "struct.rkt"
         (only-in "syntax.rkt" resolve [id=? stx:id=?]))
(provide (all-defined-out))

;; ----------------------------------------
;; Simple parsing of already-expanded code

;; This parse is the same as the single-phase one, but with `ph`
;; threaded through to `resolve`
;(: parse : Ph Stx Σ -> Ast)
(define ((parse/parse*/resolve parse* resolve) ph stx Σ)
  (define (id=? nam) (λ (id) (stx:id=? ph id nam Σ)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'lambda)))
               ,(GenStx stl_ids _) ,stx_body) _)
     (Fun (map (λ (id) (Var (resolve ph id Σ)))
               stl_ids)
          ((parse/parse*/resolve parse* resolve) ph stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'let)))
               ,(GenStx (? ProperStl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (Fun (map (λ (id) (Var (resolve ph id Σ)))
                      stl_ids)
                 ((parse/parse*/resolve parse* resolve) ph stx_body Σ))
            (map (λ (stx_rhs) ((parse/parse*/resolve parse* resolve) ph stx_rhs Σ))
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
     (App ((parse/parse*/resolve parse* resolve) ph stx_fun Σ)
          ((parse*) ph stl_args Σ))]
    ; (if stx stx stx)
    [(GenStx `(,(? Id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (If ((parse/parse*/resolve parse* resolve) ph stx_test Σ)
         ((parse/parse*/resolve parse* resolve) ph stx_then Σ)
         ((parse/parse*/resolve parse* resolve) ph stx_else Σ))]
    ; reference
    [(? Id? id) (Var (resolve ph id Σ))]
    ; literal
    [(GenStx (? Atom? atom) _) atom]))

;(: parse* : Ph Stl Σ -> (Listof Ast))
(define ((parse*/parse parse) ph stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons ((parse) ph stx Σ) ((parse*/parse parse) ph stl Σ))]
    [stx (list ((parse) ph stx Σ))]))

(define (parse&parse*/resolve resolve)
  (letrec ([parse  (λ () (parse/parse*/resolve parse* resolve))]
           [parse* (λ () (parse*/parse parse))])
    (values (parse) (parse*))))

(define-values (parse parse*) (parse&parse*/resolve resolve))
