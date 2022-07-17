#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt"  use-terms)
 (only-in "../../terms.rkt" use-lst-form)

 (only-in "../../signatures.rkt"
          terms-extra^ syntax^ delta^ menv^ bind^ parse^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import (only terms^
              Var% Fun% App% If% Atom% Stx% List% Null% Pair% Prim%)
        (only terms-extra^
              lst->list id? proper-stl?)
        (only syntax^
              unzip strip)
        (only delta^
              prim?)
        (only menv^
              init-ξ)
        (rename (only bind^
                      resolve id=?)
                [b:id=? id=?]))
(export parse^)

(use-terms Var Fun App If Atom Stx List Null Pair Prim)
(use-lst-form Lst List? Null Pair lst->list)

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
  (define (id=? nam) (λ (id) (b:id=? #:phase ph id nam #:ξ (init-ξ) Σ)))

  (lift
   (match stx
     ; (lambda (id ...) stx_body)
     [(Stx (Lst (? id? (? (id=? 'lambda)))
                (Stx stl_ids _)
                stx_body) _)
      (for*/set ([var_list
                  (in-set (build-alt-lists
                           (for/list ([id (in-list (lst->list stl_ids))])
                             (for/list
                                 ([nam (in-set
                                        (results (do (resolve #:phase ph
                                                              id Σ))))])
                               (Var nam)))))]
                 [ast_body (results (do (parse #:phase ph stx_body Σ)))])
        (Fun var_list ast_body))]
     ; (let ([id stx_rhs] ...) stx_body)
     [(Stx (Lst (? id? (? (id=? 'let)))
                (Stx (? proper-stl?  stl_binds) _)
                stx_body) _)
      (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
        (for*/set
            ([var_list (in-set (build-alt-lists
                                (for/list ([id (in-list (lst->list stl_ids))])
                                  (for/set
                                    ([nam (in-set
                                           (results (do (resolve #:phase ph
                                                                 id Σ))))])
                                    (Var nam)))))]
             [rhs_list (in-set (build-alt-lists
                                (for/list ([stx (in-list (lst->list stl_rhs))])
                                  (for/set ([ast
                                             (in-set
                                              (results (do (parse #:phase ph
                                                                  stx Σ))))])
                                    ast))))]
             [ast_body (in-set (results (do (parse #:phase ph stx_body Σ))))])
          (App (gensym 'let) (Fun var_list ast_body) rhs_list)))]
     ; (quote stx)
     [(Stx (Lst (? id? (? (id=? 'quote))) stx) _)
      (set (let ([datum (strip stx)])
             (if (prim? datum)
                 (Prim datum)
                 datum)))]
     ; (syntax stx)
     [(Stx (Lst (? id? (? (id=? 'syntax))) stx) _)
      (set stx)]
     ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
     [(Stx (Pair (? id? (? (id=? '#%app)))
                 (Stx (Pair stx_fun stl_args) _)) _)
      (for*/set ([ast_fun  (in-set (results (do (parse #:phase ph stx_fun Σ))))]
                 [ast_args (in-set (parse* #:phase ph stl_args Σ))])
        (App (gensym 'app) ast_fun ast_args))]
     ; (if stx stx stx)
     [(Stx (Lst (? id? (? (id=? 'if))) stx_test stx_then stx_else) _)
      (for*/set
          ([ast_test (in-set (results (do (parse #:phase ph stx_test Σ))))]
           [ast_then (in-set (results (do (parse #:phase ph stx_then Σ))))]
           [ast_else (in-set (results (do (parse #:phase ph stx_else Σ))))])
        (If (gensym 'if) ast_test ast_then ast_else))]
     ; reference
     [(? id? id)
      (for/set ([nam (in-set (results (do (resolve #:phase ph id Σ))))])
        (Var nam))]
     ; literal
     [(Stx (? Atom? a) _) (set a)])))

; parse* : Ph Stl Σ -> (Setof (Listof Ast))
(define (parse* #:phase [ph #f] stl Σ)
  (match stl
    [(Null) (set '())]
    [(Pair stx stl)
     (for*/set ([ast (in-set (results (do (parse #:phase ph stx Σ))))]
                [asts (in-set (parse* #:phase ph stl Σ))])
       (cons ast asts))]
    [stx (for/set ([ast (in-set (results (do (parse #:phase ph stx Σ))))])
           (list ast))]))
