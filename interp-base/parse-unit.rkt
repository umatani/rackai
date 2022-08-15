#lang racket/unit
(require
 racket/match
 (only-in "../term.rkt"  use-terms)
 
 (only-in "../signatures.rkt" syntax^ menv^ bind^ parse^)
 (only-in "../terms.rkt" #%term-forms
          Var% Fun% App% If% Atom% Bool% Num% Sym% Stx% Null% Pair% Prim%
          Lst lst->list id? prim? proper-stl?))

(import
 (only syntax^
       strip unzip)
 (only menv^
       init-ξ)
 (rename (only bind^
               resolve id=?)
         [b:id=? id=?]))
(export parse^)

(use-terms Var Fun App If Atom Bool Num Sym Stx Null Pair Prim)

;; ----------------------------------------
;; Simple parsing of already-expanded code

(define (parse #:phase [ph #f] stx Σ)
  (define (id=? nam) (λ (id) (b:id=? #:phase ph id nam #:ξ (init-ξ) Σ)))

  (match stx

    ; (lambda (id ...) stx_body)
    [(Stx (Lst (? id? (? (id=? 'lambda)))
               (Stx stl_ids _)
               stx_body) _)
     (Fun (map (λ (id) (Var (resolve #:phase ph id Σ)))
               (lst->list stl_ids))
          (parse #:phase ph stx_body Σ))]

    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx (Lst (? id? (? (id=? 'let)))
               (Stx (? proper-stl?  stl_binds) _)
               stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (gensym 'let)
            (Fun (map (λ (id) (Var (resolve #:phase ph id Σ)))
                      (lst->list stl_ids))
                 (parse #:phase ph stx_body Σ))
            (parse* #:phase ph stl_rhs Σ)))]

    ; (quote stx)
    [(Stx (Lst (? id? (? (id=? 'quote))) stx) _)
     (let ([datum (strip stx)])
       (if (prim? datum)
           (Prim datum stx) ;; stx is used for alloc-box, alloc-def-ξ
           datum))]

    ; (syntax stx)
    [(Stx (Lst (? id? (? (id=? 'syntax))) stx) _)
     stx]

    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(Stx (Pair (? id? (? (id=? '#%app)))
                (Stx (Pair stx_fun stl_args) _)) _)
     (App (gensym 'app)
          (parse  #:phase ph stx_fun Σ)
          (parse* #:phase ph stl_args Σ))]

    ; (if stx stx stx)
    [(Stx (Lst (? id? (? (id=? 'if))) stx_test stx_then stx_else) _)
     (If (gensym 'if)
         (parse #:phase ph stx_test Σ)
         (parse #:phase ph stx_then Σ)
         (parse #:phase ph stx_else Σ))]

    ; reference
    [(? id? id) (Var (resolve #:phase ph id Σ))]

    ; literal
    [(Stx (? Atom? a) _) a]))

; parse* : Ph Stl Σ -> (Listof Ast)
(define (parse* #:phase [ph #f] stl Σ)
  (match stl
    [(Null) '()]
    [(Pair stx stl)
     (cons (parse #:phase ph stx Σ) (parse* #:phase ph stl Σ))]
    [(? Stx? stx)
     (list (parse #:phase ph stx Σ))]))
