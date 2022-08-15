#lang racket
(require
 "../../set.rkt"
 "../../reduction.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt"
          syntax^ env^ store^ cont^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^)
 (only-in "../../interp-base/full/terms.rkt" #%term-forms
          Var% Fun% App% If% Bool% VFun% Sym% Stx% Null% Pair% Prim% Defs% 𝓁%
          Stxξ%
          KApp% KIf% SApp% SIf% AstEnv% Σ% ζ% Σ*% TVar% TStop% InExpand%
          lst->list id? stx-prim? val?)
 (only-in "../../interp-base/full/eval.rkt" [--> base:-->]))
(provide --> red@ eval@)

;; --> : State -> (Setof State)
(define-reduction (--> delta ==>) #:super (base:--> delta ==> <-)
  #:within-signatures [(only syntax^
                             add flip union alloc-scope prune)
                       (only env^
                             init-env lookup-env extend-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-𝓁 lookup-Σ update-Σ)
                       (only bind^
                             bind resolve)
                       (only parser^
                             parse)]
  #:do [; resolve* : Ph (Listof Id) Σ -> (SetM (Listof Nam))
        (define (resolve* ph ids Σ)
          (match ids
            ['() (pure '())]
            [(cons id ids*)
             (do nam  <- (resolve #:phase ph id Σ)
                 nams <- (resolve* ph ids* Σ)
                 (pure (cons nam nams)))]))

        ;; lookup-ξ* : ξ (Listof Nam) -> (SetM (Listof AllTransform))
        (define (lookup-ξ* ξ ns)
          (match ns
            ['() (pure '())]
            [(cons n ns*)
             (do  a <- (let ([as (lookup-ξ  ξ n)])
                         (if (set-empty? (results as))
                             (pure 'not-found)
                             as))
                 as <- (lookup-ξ* ξ ns*)
                 (pure (cons a as)))]))])

(define-unit-from-reduction red@ -->)

(define-mixed-unit eval@
  (import (only env^
                init-env)
          (only store^
                init-store)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                ==>))
  (export eval^)
  (inherit [red@ reducer])
  (use-terms AstEnv Σ*)

  (define (--> delta) (λ () (reducer delta (==> delta))))

  ; eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*))
  (define (eval delta ph ast maybe-scp_i ξ Σ*)
    (define -->d (--> delta))
    (match-let ([(set `(,(? val? val) • ,_store ,Σ*_2) ...)
                 (apply-reduction-relation*
                  (-->d) `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                           • ,(init-store) ,Σ*))])
      (list->set (map cons val Σ*_2))))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate delta ast)
    (for/set ([val+Σ* (in-set (eval delta 0 ast 'no-scope (init-ξ)
                                     (Σ* (init-Σ) (set) (set))))])
      (car val+Σ*))))
