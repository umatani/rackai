#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt" use-terms)
 (only-in "../../../dprint.rkt" dprint)
 
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 (only-in "../../base/full/eval.rkt" [--> base:-->]))
(provide --> eval@)

;; --> : State -> (Setof State)
(define-reduction (--> delta ==>) #:super (base:--> delta ==> <-)
  #:within-signatures [(only terms^
                             Var% Fun% App% If% VFun% Sym% Stx% AstEnv%
                             Stxξ% Σ% Σ*% 𝓁% InExpand% ζ%
                             KApp% KIf% SApp% SIf% TVar% TStop% Defs%)
                       (only terms-extra^
                             val? id? prim? stx-prim?)
                       (only syntax^
                             add flip union prune)
                       (only env^
                             init-env lookup-env update-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope)
                       (only bind^
                             bind resolve)
                       (only parser^
                             parse)]
  #:do [; resolve* : Ph (Listof Id) Σ -> (SetM (Listof Nam))
        (define (resolve* ph val Σ)
          (match val
            ['() (pure '())]
            [(cons id val2)
             (do nam  <- (resolve #:phase ph id Σ)
                 nams <- (resolve* ph val2 Σ)
                 (pure (cons nam nams)))]))
        ])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import (only terms^
                AstEnv% Σ*%)
          (only terms-extra^
                val?)
          (only env^
                init-env)
          (only store^
                init-store)
          (only delta^
                delta)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                ==>))
  (export eval^)
  (inherit [red@ reducer])

  (use-terms AstEnv Σ*)

  (define --> (λ () (reducer delta ==>)))

  ; eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*))
  (define (eval ph ast maybe-scp_i ξ Σ*)
    (match-let ([(set `(,(? val? val) • ,_store ,Σ*_2) ...)
                 (apply-reduction-relation*
                  (-->) `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                          • ,(init-store) ,Σ*))])
      (list->set (map cons val Σ*_2))))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate ast)
    (for/set ([val+Σ*
               (in-set (eval 0 ast 'no-scope (init-ξ)
                             (Σ* (init-Σ) (set) (set))))])
      (car val+Σ*))))
