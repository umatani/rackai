#lang racket
(require
 "../../reduction.rkt"
 "../../test/suites.rkt"

 (only-in "../../signatures.rkt" syntax^ env^ store^ domain^
          menv^ mstore^ bind^ mcont^ parser^ run^ debug^)
 (only-in "../../interp-base/phases/terms.rkt"
          App% Atom% Sym% Stx% List% Null% Pair% AstEnv%
          Stxξ% κ% ζ% TVar% InEval% Hole%
          Lst lst->list snoc id? prim?)
 (only-in "../../interp-set/phases/units.rkt" expand/red@)
 (only-in "../core.rkt" eval/red@)
 (only-in "../phases.rkt" [==> abs:==>] main-minus@)
 (only-in "domain.rkt" domain@ stx-⊤ val? proper-stl?)
 (only-in "core.rkt" ev:red@))
(provide run delta α ≤a)

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (abs:==> -->)
  #:within-signatures [(only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             alloc-scope prune at-phase)
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

  [(InEval (list (? Stx? stx_exp) '• store_0)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ scps_p) '∘ κ0 Σ))
   (if (equal? stx_exp stx-⊤)
       (ζ (Stxξ ph stx_exp ξ scps_p) '∘ κ0 Σ)
       (let ([scp_i (car (set->list (at-phase ctx_i ph)))])
         (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ scps_p) '∘ κ0 Σ)))
   ex-macapp-flip]

  ;; stx-⊤
  [(ζ (Stxξ ph (and stx (Stx 'stx-⊤ ctx)) _ _) '∘ κ0 Σ)
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
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a))
