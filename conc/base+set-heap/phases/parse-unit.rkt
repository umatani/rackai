#lang racket/unit
(require (except-in racket set do)
         "../../../set.rkt" "../../../nondet.rkt"
         (only-in "../parse-util.rkt" build-alt-lists)

         (only-in "../../../struct-common-sig.rkt" struct-common^)
         (only-in "../../../syntax-sig.rkt" syntax^)
         (only-in "../../../mstore-sig.rkt" mstore^)

         (only-in "../../../parse-sig.rkt" parse^))

(import ;; from conc/base/phases
        (only struct-common^
              Stx id? atom? var fun iif app proper-stl?)

        ;; set-based version from conc/base+set-heap/core
        (only syntax^ unzip strip)
        (rename (only mstore^
                      resolve id=?) [core:id=? id=?]))
(export parse^)

;; Non-deterministic parsing

;; This parse is the same as the single-phase one, but with `ph`
;; threaded through to `resolve`

;(: parse : Ph Stx Σ -> (SetM Ast))
(define (parse ph stx Σ)
  (define (id=? nam) (λ (id) (core:id=? ph id nam Σ)))
  (lift
   (match stx
     ; (lambda (id ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'lambda)))
             ,(Stx stl_ids _) ,stx_body) _)
      (for*/set ([var_list (in-set (build-alt-lists
                                    (for/list ([id (in-list stl_ids)])
                                      (for/list
                                         ([nam (in-set
                                                (car (do (resolve ph id Σ))))])
                                        (var nam)))))]
                 [ast_body (car (do (parse ph stx_body Σ)))])
        (fun var_list ast_body))]
     ; (let ([id stx_rhs] ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'let)))
             ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
      (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
        (for*/set
            ([var_list (in-set (build-alt-lists
                                (for/list ([id (in-list stl_ids)])
                                  (for/set
                                    ([nam (in-set (car (do (resolve ph id Σ))))])
                                    (var nam)))))]
             [rhs_list (in-set (build-alt-lists
                                (for/list ([stx (in-list stl_rhs)])
                                  (for/set ([ast (in-set
                                                  (car (do (parse ph stx Σ))))])
                                    ast))))]
             [ast_body (in-set (car (do (parse ph stx_body Σ))))])
          (app (fun var_list ast_body) rhs_list)))]
     ; (quote stx)
     [(Stx `(,(? id? (? (id=? 'quote))) ,stx) _)
      (set (strip stx))]
     ; (syntax stx)
     [(Stx `(,(? id? (? (id=? 'syntax))) ,stx) _)
      (set stx)]
     ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
     [(Stx (cons (? id? (? (id=? '#%app)))
                 (Stx (cons stx_fun stl_args) _)) _)
      (for*/set ([ast_fun  (in-set (car (do (parse ph stx_fun Σ))))]
                 [ast_args (in-set (parse* ph stl_args Σ))])
        (app ast_fun ast_args))]
     ; (if stx stx stx)
     [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
      (for*/set ([ast_test (in-set (car (do (parse ph stx_test Σ))))]
                 [ast_then (in-set (car (do (parse ph stx_then Σ))))]
                 [ast_else (in-set (car (do (parse ph stx_else Σ))))])
        (iif ast_test ast_then ast_else))]
     ; reference
     [(? id? id)
      (for/set ([nam (in-set (car (do (resolve ph id Σ))))]) (var nam))]
     ; literal
     [(Stx (? atom? atom) _) (set atom)])))

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

; parser : Stx Σ -> Ast
(define (parser stx Σ) (parse 0 stx Σ))
