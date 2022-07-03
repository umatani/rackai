#lang racket/unit
(require
 racket/match
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt"
          terms-extra^ syntax^ menv^ mstore^ parse^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import
 (only terms^
       Var% Fun% App% If% Stx%)
 (only terms-extra^
       atom? id? proper-stl?)
 (only syntax^
       strip unzip)
 (only menv^
       init-ξ)
 (rename (only mstore^
               resolve id=?) [msto:id=? id=?]))
(export parse^)

(use-terms Var Fun App If Stx)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define (parse #:phase [ph #f] stx Σ)
  (define (id=? nam) (λ (id) (msto:id=? #:phase ph id nam #:ξ (init-ξ) Σ)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'lambda)))
            ,(Stx stl_ids _) ,stx_body) _)
     (Fun (map (λ (id) (Var (resolve #:phase ph id Σ)))
               stl_ids)
          (parse #:phase ph stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'let)))
            ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (Fun (map (λ (id) (Var (resolve #:phase ph id Σ)))
                      stl_ids)
                 (parse #:phase ph stx_body Σ))
            (map (λ (stx_rhs) (parse #:phase ph stx_rhs Σ))
                 stl_rhs)))]
    ; (quote stx)
    [(Stx `(,(? id? (? (id=? 'quote))) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(Stx `(,(? id? (? (id=? 'syntax))) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(Stx (cons (? id? (? (id=? '#%app)))
                (Stx (cons stx_fun stl_args) _)) _)
     (App (parse #:phase ph stx_fun Σ)
          (parse* #:phase ph stl_args Σ))]
    ; (if stx stx stx)
    [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (If (parse #:phase ph stx_test Σ)
         (parse #:phase ph stx_then Σ)
         (parse #:phase ph stx_else Σ))]
    ; reference
    [(? id? id) (Var (resolve #:phase ph id Σ))]
    ; literal
    [(Stx (? atom? atom) _) atom]))

; parse* : Ph Stl Σ -> (Listof Ast)
(define (parse* #:phase [ph #f] stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse #:phase ph stx Σ) (parse* #:phase ph stl Σ))]
    [stx (list (parse #:phase ph stx Σ))]))
