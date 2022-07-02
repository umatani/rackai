#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt"        use-terms)

 (only-in "../../terms.rkt"       terms^ #%term-forms)
 (only-in "../../terms-extra.rkt" terms-extra^)
 (only-in "../../syntax-sig.rkt"  syntax^)
 (only-in "../../menv-sig.rkt"    menv^)
 (only-in "../../mstore-sig.rkt"  mstore^)
 (only-in "../../parse-sig.rkt"   parse^))

(import (only terms^
              Var% Fun% App% If% Stx%)
        (only terms-extra^
              id? atom? proper-stl?)
        (only syntax^
              unzip strip)
        (only menv^
              init-ξ)
        (rename (only mstore^
                      resolve id=?)
                [msto:id=? id=?]))
(export parse^)

(use-terms Var Fun App If Stx)

;; ----------------------------------------
;; Simple parsing of already-expanded code

; build-alt-lists : (Listof (Setof A)) -> (Setof (Listof A))
(define (build-alt-lists alts-list)
  (if (null? alts-list)
      (set '())
      (let ([lsts (build-alt-lists (cdr alts-list))])
        (for*/set ([alt (in-set (car alts-list))]
                   [lst (in-set lsts)])
          (cons alt lst)))))


; parse : Ph Stx Σ -> (SetM Ast)
(define (parse #:phase [ph #f] stx Σ)
  (define (id=? nam) (λ (id) (msto:id=? #:phase ph id nam #:ξ (init-ξ) Σ)))
  (lift
   (match stx
     ; (lambda (id ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'lambda)))
             ,(Stx stl_ids _) ,stx_body) _)
      (for*/set ([var_list (in-set (build-alt-lists
                                    (for/list ([id (in-list stl_ids)])
                                      (for/list
                                         ([nam (in-set
                                                (car (do (resolve #:phase ph
                                                                  id Σ))))])
                                        (Var nam)))))]
                 [ast_body (car (do (parse #:phase ph stx_body Σ)))])
        (Fun var_list ast_body))]
     ; (let ([id stx_rhs] ...) stx_body)
     [(Stx `(,(? id? (? (id=? 'let)))
             ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
      (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
        (for*/set
            ([var_list (in-set (build-alt-lists
                                (for/list ([id (in-list stl_ids)])
                                  (for/set
                                    ([nam (in-set
                                           (car (do (resolve #:phase ph
                                                             id Σ))))])
                                    (Var nam)))))]
             [rhs_list (in-set (build-alt-lists
                                (for/list ([stx (in-list stl_rhs)])
                                  (for/set ([ast
                                             (in-set
                                              (car (do (parse #:phase ph
                                                              stx Σ))))])
                                    ast))))]
             [ast_body (in-set (car (do (parse #:phase ph stx_body Σ))))])
          (App (Fun var_list ast_body) rhs_list)))]
     ; (quote stx)
     [(Stx `(,(? id? (? (id=? 'quote))) ,stx) _)
      (set (strip stx))]
     ; (syntax stx)
     [(Stx `(,(? id? (? (id=? 'syntax))) ,stx) _)
      (set stx)]
     ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
     [(Stx (cons (? id? (? (id=? '#%app)))
                 (Stx (cons stx_fun stl_args) _)) _)
      (for*/set ([ast_fun  (in-set (car (do (parse #:phase ph stx_fun Σ))))]
                 [ast_args (in-set (parse* #:phase ph stl_args Σ))])
        (App ast_fun ast_args))]
     ; (if stx stx stx)
     [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
      (for*/set ([ast_test (in-set (car (do (parse #:phase ph stx_test Σ))))]
                 [ast_then (in-set (car (do (parse #:phase ph stx_then Σ))))]
                 [ast_else (in-set (car (do (parse #:phase ph stx_else Σ))))])
        (If ast_test ast_then ast_else))]
     ; reference
     [(? id? id)
      (for/set ([nam (in-set (car (do (resolve #:phase ph id Σ))))]) (Var nam))]
     ; literal
     [(Stx (? atom? atom) _) (set atom)])))

; parse* : Ph Stl Σ -> (Setof (Listof Ast))
(define (parse* #:phase [ph #f] stl Σ)
  (match stl
    ['() (set '())]
    [(cons stx stl)
     (for*/set ([ast (in-set (car (do (parse #:phase ph stx Σ))))]
                [asts (in-set (parse* #:phase ph stl Σ))])
       (cons ast asts))]
    [stx (for/set ([ast (in-set (car (do (parse #:phase ph stx Σ))))])
           (list ast))]))
