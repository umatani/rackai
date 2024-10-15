#lang racket
(require
 (only-in "../../mix.rkt"   define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt")
(provide parse@ parser@)

(define-unit parse@
  (import
   (only domain^    proper-stl?)
   (only syntax^    unzip strip)
   (only   bind^    resolve core-form?))
  (export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

  ;; parse1 : Ph Stx Σ → Ast
  (define (parse1 ph stx Σ)
    (match stx
      ; (lambda (id ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? ph 'lambda Σ)))
                 (Stx stl_ids _)
                 stx_body) _)
       (Fun (map (λ (id) (Var (resolve ph id Σ)))
                 (lst->list stl_ids))
            (parse1 ph stx_body Σ))]

      ; (let ([id stx_rhs] ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? ph 'let Σ)))
                 (Stx (? proper-stl?  stl_binds) _)
                 stx_body) _)
       (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
         (App (gensym 'let)
              (Fun (map (λ (id) (Var (resolve ph id Σ)))
                        (lst->list stl_ids))
                   (parse1 ph stx_body Σ))
              (parse* ph stl_rhs Σ)))]

      ; (quote stx)
      [(Stx (Lst (? id? (? (core-form? ph 'quote Σ))) stx) _)
       (let ([datum (strip stx)])
         (if (prim? datum)
           (Prim datum stx) ;; stx is used for alloc-box, alloc-def-ξ
           datum))]

      ; (syntax stx)
      [(Stx (Lst (? id? (? (core-form? ph 'syntax Σ))) stx) _)
       stx]

      ; (#%app stx_fun stx_arg ...) Note that it is non-proper (i.e. pair of stxs)
      [(Stx (Pair (? id? (? (core-form? ph '#%app Σ)))
                  (Stx (Pair stx_fun stl_args) _)) _)
       (App (gensym 'app)
            (parse1 ph stx_fun Σ)
            (parse* ph stl_args Σ))]

      ; (if stx stx stx)
      [(Stx (Lst (? id? (? (core-form? ph 'if Σ))) stx_test stx_then stx_else) _)
       (If (gensym 'if)
           (parse1 ph stx_test Σ)
           (parse1 ph stx_then Σ)
           (parse1 ph stx_else Σ))]

      ; reference
      [(? id? id) (Var (resolve ph id Σ))]

      ; literal
      [(Stx (? Atom? a) _) a]))

  ;; parse* : Ph Stl Σ → (Listof Ast)
  (define (parse* ph stl Σ)
    (match stl
      [(Null) '()]
      [(Pair stx stl)
       (cons (parse1 ph stx Σ) (parse* ph stl Σ))]
      [(? Stx? stx)
       (list (parse1 ph stx Σ))]))

  ;; parse : Ph Stx Σ → Ast
  (define parse parse1))

(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@ parse])

  ; parser : Stx Σ → Ast
  (define (parser stx Σ) (parse 0 stx Σ)))
