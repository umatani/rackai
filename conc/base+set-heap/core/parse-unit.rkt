#lang racket/unit
(require (except-in racket set do)
         "../../base/set.rkt"
         "../../base/nondet.rkt"

         "../../base/struct-sig.rkt"
         "../../base/syntax-sig.rkt"
         "../../base/parse-sig.rkt")

(import (only struct^
              Stx proper-stl? var fun app iif atom? id?)
        (rename (only syntax^
                      unzip strip resolve id=?)
                [stx:id=? id=?]))
(export parse^)


;; Non-deterministic parsing

;(: build-alt-lists : (Listof (Setof A)) -> (Setof (Listof A)))
(define (build-alt-lists alts-list)
  (if (null? alts-list)
      (set '())
      (let ([lsts (build-alt-lists (cdr alts-list))])
        (for*/set ([alt (in-set (car alts-list))]
                   [lst (in-set lsts)])
          (cons alt lst)))))


;(: parse : Stx Σ -> (SetM Ast))
(define (parse stx Σ)
  (define (id=? nam) (λ (id) (stx:id=? id nam Σ)))
  (lift
   (match stx
     ; (lambda (id ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'lambda)))
             ,(Stx stl_ids _) ,stx_body) _)
      (for*/set ([var_list (in-set
                            (build-alt-lists
                             (for/list ([id (in-list stl_ids)])
                               (for/list
                                  ([nam (in-set
                                         (car (do (resolve id Σ))))])
                                 (var nam)))))]
                 [ast_body (car (do(parse stx_body Σ)))])
        (fun var_list ast_body))]
     ; (let ([id stx_rhs] ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'let)))
             ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
      (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
        (for*/set
            ([var_list (in-set (build-alt-lists
                                (for/list ([id (in-list stl_ids)])
                                  (for/set
                                      ([nam (in-set
                                             (car (do (resolve id Σ))))])
                                    (var nam)))))]
             [rhs_list (in-set (build-alt-lists
                                (for/list ([stx (in-list stl_rhs)])
                                  (for/set ([ast (in-set
                                                  (car (do (parse stx Σ))))])
                                    ast))))]
             [ast_body (in-set (car (do (parse stx_body Σ))))])
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
      (for*/set ([ast_fun  (in-set (car (do (parse stx_fun Σ))))]
                 [ast_args (in-set (parse* stl_args Σ))])
        (app ast_fun ast_args))]
     ; (if stx stx stx)
     [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
      (for*/set ([ast_test (in-set (car (do (parse stx_test Σ))))]
                 [ast_then (in-set (car (do (parse stx_then Σ))))]
                 [ast_else (in-set (car (do (parse stx_else Σ))))])
        (iif ast_test ast_then ast_else))]
     ; reference
     [(? id? id)
      (for/set ([nam (in-set (car (do (resolve id Σ))))])
        (var nam))]
     ; literal
     [(Stx (? atom? atom) _) (set atom)])))

;(: parse* : Stl Σ -> (Setof (Listof Ast)))
(define (parse* stl Σ)
  (match stl
    ['() (set '())]
    [(cons stx stl)
     (for*/set ([ast (in-set (car (do (parse stx Σ))))]
                [asts (in-set (parse* stl Σ))])
       (cons ast asts))]
    [stx (for/set ([ast (in-set (car (do (parse stx Σ))))])
           (list ast))]))

(define parser parse)
