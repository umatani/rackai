#lang racket
(require
 "../../set.rkt"
 "../../reduction.rkt"
 "../../mix.rkt"
 (only-in "../../term.rkt" use-terms)
 "../../test/suites.rkt"

 (only-in "../../signatures.rkt"
          syntax^ env^ store^ cont^ domain^ eval^
          menv^ mstore^ mcont^ bind^ parser^ expand^ run^ debug^)
 (only-in "../../terms.rkt" [#%term-forms tm:#%term-forms]
          Var% Fun% App% If% Val% Atom% List% VFun% Bool% Sym%
          Stx% Stxξ% Null% Pair% Prim% Hole%
          Lst snoc id? prim?)

 (only-in "../../interp-base/core/units.rkt" debug@ expander@)
 (only-in "../../interp-base/core/config.rkt"
          config^ config@ [#%term-forms cfg:#%term-forms])
 (only-in "../../interp-set/core/units.rkt" expand/red@)
 (only-in "../../interp-set/core/eval.rkt" [--> set:-->])
 (only-in "../core.rkt" [==> abs:==>] main-minus@)

 (only-in "domain.rkt"
          domain@ val-⊤ atom-⊤ stx-⊤
          val? proper-stl? stx?))
(provide run delta α ≤a)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))

;; Revise --> to interpret abstract values (val-⊤, stx-⊤, etc.)
;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (set:--> delta)
  #:within-signatures [(only config^
                             SApp% SIf% KApp% KIf% AstEnv%)
                       (only env^
                             extend-env lookup-env)
                       (only store^
                             update-store* lookup-store alloc-loc*)
                       (only cont^
                             push-cont)]
  ;; β
  [`(,(SApp _lbl (cons f _) '()) ,cont ,store)
   #:when (equal? f val-⊤)
   `(,f ,cont ,store)
   ev-β-abs]

  ;; (if ⊤ ...)
  [`(,(SIf _lbl (? (λ (v) (or (equal? v val-⊤) (equal? v atom-⊤))))
           _ tm_else) ,cont ,store)
   `(,tm_else ,cont ,store)
   ev-if-abs-#f])

(define-unit-from-reduction ev:red@ -->)

;; filter out stuck states (same as ../core.rkt)
(define-mixed-unit eval@
  (import (only config^
                AstEnv%)
          (only env^
                init-env)
          (only store^
                init-store))
  (export eval^)
  (inherit [ev:red@ reducer])
  (use-terms AstEnv)

  (define (--> delta) (reducer delta))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate delta ast)
    (define -->d (--> delta))
    (match-let ([(set `(,val ,done? ,_store) ...)
                 (apply-reduction-relation*
                  -->d `(,(AstEnv ast (init-env)) • ,(init-store)))])
      (list->set
       (map car
            (filter (λ (vd) (and (val? (car vd)) (eq? (cdr vd) '•)))
                    (map cons val done?)))))))


;; Revise reduction rule ==>

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (abs:==> -->)
  #:within-signatures [(only config^
                             AstEnv% TVar% ζ% κ% InEval%)
                       (only syntax^
                             empty-ctx zip unzip alloc-scope add flip in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parser^
                             parse)]

  [(InEval (list (? stx? stx_exp) '• store_0)
           (ζ (Stxξ (Stx #f scps) ξ) '∘ κ Σ))
   (if (equal? stx_exp stx-⊤)
       (ζ (Stxξ stx_exp ξ) '∘ κ Σ)
       (let ([scp_i (car (set->list scps))])
         (ζ (Stxξ (flip stx_exp scp_i) ξ) '∘ κ Σ)))
   ex-macapp-flip]

  ;; stx-⊤
  [(ζ (Stxξ (Stx 'stx-⊤ ctx) ξ) '∘ κ Σ)
   (ζ (Stx 'stx-⊤ ctx) '• κ Σ)
   ex-stx-⊤])

(define-unit-from-reduction ex:red@ ==>)

(define-compound-unit/infer expand@
  (import syntax^ config^ env^ store^ eval^ menv^ mstore^
          mcont^ bind^ parser^)
  (export expand^)
  (link expand/red@ ex:red@))

;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ main-minus@ eval@ expand@ expander@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core) mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a))
