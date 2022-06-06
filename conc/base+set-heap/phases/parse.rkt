#lang racket
(require "../../base/nondet.rkt"
         (only-in "../../base/phases/parse.rkt" parse&parse*/resolve)
         "../../base/phases/struct.rkt"
         (only-in "../../base/core/syntax.rkt" unzip strip)

         ;; Set-based version
         (only-in "../core/parse.rkt" build-alt-lists)
         (only-in "syntax.rkt" resolve [id=? stx:id=?]))

(provide (all-defined-out))

;; Non-deterministic parsing

;; This parse is the same as the single-phase one, but with `ph`
;; threaded through to `resolve`

;(: parse : Ph Stx Σ -> (SetM Ast))
(define (parse ph stx Σ)
  (define (id=? nam) (λ (id) (stx:id=? ph id nam Σ)))
  (lift
   (match stx
     ; (lambda (id ...) stx_body)
     [(GenStx `(,(? Id? (? (id=? 'lambda)))
                ,(GenStx stl_ids _) ,stx_body) _)
      (for*/set ([var_list (in-set (build-alt-lists
                                    (for/list ([id (in-list stl_ids)])
                                      (for/list
                                         ([nam (in-set
                                                (car (do (resolve ph id Σ))))])
                                        (Var nam)))))]
                 [ast_body (car (do (parse ph stx_body Σ)))])
        (Fun var_list ast_body))]
     ; (let ([id stx_rhs] ...) stx_body)
     [(GenStx `(,(? Id? (? (id=? 'let)))
                ,(GenStx (? ProperStl?  stl_binds) _) ,stx_body) _)
      (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
        (for*/set
            ([var_list (in-set (build-alt-lists
                                (for/list ([id (in-list stl_ids)])
                                  (for/set
 
 ([nam (in-set (car (do (resolve ph id Σ))))])
                                    (Var nam)))))]
             [rhs_list (in-set (build-alt-lists
                                (for/list ([stx (in-list stl_rhs)])
                                  (for/set ([ast (in-set
                                                  (car (do (parse ph stx Σ))))])
                                    ast))))]
             [ast_body (in-set (car (do (parse ph stx_body Σ))))])
          (App (Fun var_list ast_body) rhs_list)))]
     ; (quote stx)
     [(GenStx `(,(? Id? (? (id=? 'quote))) ,stx) _)
      (set (strip stx))]
     ; (syntax stx)
     [(GenStx `(,(? Id? (? (id=? 'syntax))) ,stx) _)
      (set stx)]
     ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
     [(GenStx (cons (? Id? (? (id=? '#%app)))
                    (GenStx (cons stx_fun stl_args) _)) _)
      (for*/set ([ast_fun  (in-set (car (do (parse ph stx_fun Σ))))]
                 [ast_args (in-set (parse* ph stl_args Σ))])
        (App ast_fun ast_args))]
     ; (if stx stx stx)
     [(GenStx `(,(? Id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
      (for*/set ([ast_test (in-set (car (do (parse ph stx_test Σ))))]
                 [ast_then (in-set (car (do (parse ph stx_then Σ))))]
                 [ast_else (in-set (car (do (parse ph stx_else Σ))))])
        (If ast_test ast_then ast_else))]
     ; reference
     [(? Id? id)
      (for/set ([nam (in-set (car (do (resolve ph id Σ))))]) (Var nam))]
     ; literal
     [(GenStx (? Atom? atom) _) (set atom)])))

;(: parse* : Ph Stl Σ -> (Setof (Listof Ast)))
(define (parse* ph stl Σ)
  (match stl
    ['() (set '())]
    [(cons stx stl)
     (for*/set ([ast (in-set (car (do (parse ph stx Σ))))]
                [asts (in-set (parse* ph stl Σ))])
       (cons ast asts))]
    [stx (for/set ([ast (in-set (car (do (parse ph stx Σ))))])
           (list ast))]))
