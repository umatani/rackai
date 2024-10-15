#lang racket
(require
 (only-in "../../mix.rkt" define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt")
(provide parser@)

(define-unit parse@
  (import
   (only domain^    proper-stl?)
   (only syntax^    unzip strip)
   (only   bind^    resolve core-form?))
  (export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

  ; parse : Stx Σ → Ast
  (define (parse stx Σ)
    (match stx
      ; (lambda (id ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'lambda Σ)))
                 (Stx stl_ids _)
                 stx_body) _)
       (Fun (map (λ (id) (Var (resolve id Σ)))
                 (lst->list stl_ids))
            (parse stx_body Σ))]

      ; (let ([id stx_rhs] ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'let Σ)))
                 (Stx (? proper-stl?  stl_binds) _)
                 stx_body) _)
       (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
         (App (gensym 'let)
              (Fun (map (λ (id) (Var (resolve id Σ)))
                        (lst->list stl_ids))
                   (parse stx_body Σ))
              (parse* stl_rhs Σ)))]

      ; (quote stx)
      [(Stx (Lst (? id? (? (core-form? 'quote Σ))) stx) _)
       (let ([datum (strip stx)])
         (if (prim? datum)
           (Prim datum stx) ;; stx is used for alloc-box, alloc-def-ξ
           datum))]

      ; (syntax stx)
      [(Stx (Lst (? id? (? (core-form? 'syntax Σ))) stx) _)
       stx]

      ; (#%app stx_fun stx_arg ...) Note that it is non-proper (i.e. pair of stxs)
      [(Stx (Pair (? id? (? (core-form? '#%app Σ)))
                  (Stx (Pair stx_fun stl_args) _)) _)
       (App (gensym 'app)
            (parse  stx_fun Σ)
            (parse* stl_args Σ))]

      ; (if stx stx stx)
      [(Stx (Lst (? id? (? (core-form? 'if Σ))) stx_test stx_then stx_else) _)
       (If (gensym 'if)
           (parse stx_test Σ)
           (parse stx_then Σ)
           (parse stx_else Σ))]

      ; reference
      [(? id? id) (Var (resolve id Σ))]

      ; literal
      [(Stx (? Atom? a) _) a]))

  ; parse* : Stl Σ → (Listof Ast)
  (define (parse* stl Σ)
    (match stl
      [(Null) '()]
      [(Pair stx stl)
       (cons (parse stx Σ) (parse* stl Σ))]
      [(? Stx? stx)
       (list (parse stx Σ))]))
  )

(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@    parse])

  (define parser parse))
