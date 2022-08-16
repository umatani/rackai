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
 (only-in "../../interp-base/core/terms.rkt" #%term-forms
          Var% Fun% App% If% Val% Atom% List% VFun% Bool% Sym%
          Stx% Stxξ% Null% Pair% Prim% Hole%
          SApp% SIf% KApp% KIf% AstEnv% TVar% ζ% κ% InEval%
          Lst snoc id? prim? stx->datum)

 (only-in "../../interp-set/core/units.rkt" expand/red@)
 (only-in "../../interp-set/core/eval.rkt" [--> set:-->])
 (only-in "../core.rkt" eval/red@ [==> abs:==>] main-minus@)

 (only-in "domain.rkt"
          domain@ val-⊤ atom-⊤ stx-⊤
          val? stx? proper-stl?))
(provide ev:red@ run delta α ≤a)

;; Revise --> to interpret abstract values (val-⊤, stx-⊤, etc.)
;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (set:--> delta)
  #:within-signatures [(only env^
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


;; Revise reduction rule ==>

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (abs:==> -->)
  #:within-signatures [(only syntax^
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
  [(ζ (Stxξ (and stx (Stx 'stx-⊤ ctx)) ξ) '∘ κ0 Σ)
   (ζ stx '• κ0 Σ)
   ex-stx-⊤])

(define-unit-from-reduction ex:red@ ==>)

;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link main-minus@
         (() eval/red@ ev)   (([ev : red^]) ev:red@)
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core) mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a))
