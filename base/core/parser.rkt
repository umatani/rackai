#lang racket
(require
 (only-in "../../mix.rkt" define-mixed-unit)
 "../../signatures.rkt"
 "terms.rkt")
(provide parse@ parser@)

[define-unit parse@
  (import
   (only domain^    proper-stl?)
   (only syntax^    unzip strip)
   (only   bind^    resolve)
   (only     id^    core-form?))
  (export parse^)

  ;; ----------------------------------------
  ;; Simple parsing of already-expanded code

  ;; parse1 : Stx Σ → Ast
  (define (parse1 stx Σ)
    (match stx
      ; (lambda (id ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'lambda Σ)))
                 (Stx (? proper-stl? stl_ids) _)
                 stx_body) _)
       (Fun (map (λ (id) (Var (resolve id Σ)))
                 (lst->list stl_ids))
            (parse1 stx_body Σ))]

      ; (let ([id stx_rhs] ...) stx_body)
      [(Stx (Lst (? id? (? (core-form? 'let Σ)))
                 (Stx (? proper-stl? stl_binds) _)
                 stx_body) _)
       (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
         (App (gensym 'let)
              (Fun (map (λ (id) (Var (resolve id Σ)))
                        (lst->list stl_ids))
                   (parse1 stx_body Σ))
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

      ; (#%app stx_fun stx_arg ...)
      [(Stx (Pair (? id? (? (core-form? '#%app Σ)))
                  (Stx (Pair stx_fun stl_args) _)) _)
       (App (gensym 'app)
            (parse1 stx_fun Σ)
            (parse* stl_args Σ))]

      ; (if stx stx stx)
      [(Stx (Lst (? id? (? (core-form? 'if Σ))) stx_test stx_then stx_else) _)
       (If (gensym 'if)
           (parse1 stx_test Σ)
           (parse1 stx_then Σ)
           (parse1 stx_else Σ))]

      ; reference
      [(? id? id) (Var (resolve id Σ))]

      ; literal
      [(Stx (? Atom? a) _) a]))

  ;; parse* : Stl Σ → (Listof Ast)
  (define (parse* stl Σ)
    (match stl
      [(Null) '()]
      [(Pair stx stl)
       (cons (parse1 stx Σ) (parse* stl Σ))]
      [(? Stx? stx)
       (list (parse1 stx Σ))]))

  ;; parse : Stx Σ → Ast
  (define parse parse1)]

(define-unit parser@
  (import
   (only   parse^    parse))
  (export parser^)

  (define parser parse))
