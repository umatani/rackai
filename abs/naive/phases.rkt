#lang racket
(require
 "../../mix.rkt"
 "../../reduction.rkt"
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"

 (only-in "../../signatures.rkt" domain^ syntax^ env^ store^
          menv^ mstore^ bind^ mcont^ parser^ run^ debug^)
 (only-in "../../conc/phases/terms.rkt"
          App% Atom% Sym% Stx% List% Null% Pair% AstEnv%
          Stxξ% κ% ζ% TVar% InEval% Hole%
          Lst lst->list snoc id? prim?)
 (only-in "../../set/phases/units.rkt" expand/red@)
 (only-in "../core.rkt" eval/red@)
 (only-in "../phases.rkt" [==> abs:==>] main-minus@)
 (only-in "domain.rkt" domain@ val-⊤ atom-⊤ num-⊤ sym-⊤ stx-⊤ list-⊤)
 (only-in "core.rkt" ev:red@)
 (only-in "parse.rkt" parse@))
(provide interp)


(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))

  ; parser : Stx Σ -> (SetM Ast)
  (define (parser stx Σ) (parse #:phase 0 stx Σ)))

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

  [(InEval (list stx_exp '• store_0)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ scps_p) '∘ κ0 Σ))
   #:when (or (equal? stx_exp val-⊤)
              (equal? stx_exp atom-⊤)
              (equal? stx_exp stx-⊤))
   (ζ (Stxξ ph stx_exp ξ scps_p) '∘ κ0 Σ)
   ex-macapp-flip-abs]

  ;; abstract value
  [(ζ (Stxξ ph val _ _) '∘ κ0 Σ)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   (ζ val '• κ0 Σ)
   ex-abs-⊤])

(define-unit-from-reduction ex:red@ ==>)

;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         (() eval/red@ ev)   (([ev : red^]) ev:red@)
         parser@
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export domain^ run^ debug^))

(define interp (interpreter 'naive:phases run delta α ≤a #f))

;; run example
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a))
