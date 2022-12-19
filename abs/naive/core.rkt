#lang racket
(require
 "../../set.rkt"
 "../../reduction.rkt"
 "../../mix.rkt"
 "../../interpreter.rkt"
 (only-in "../../term.rkt" use-terms)
 ;"../../test/suites.rkt"

 (only-in "../../signatures.rkt"
          domain^ syntax^ env^ store^ cont^ eval^
          menv^ mstore^ mcont^ bind^ parser^ expand^ run^ debug^)
 (only-in "../../interp-base/core/terms.rkt" #%term-forms
          Var% Fun% App% If% Val% Atom% List% VFun% Bool% Sym%
          Stx% Stxξ% Null% Pair% Prim% Hole%
          SApp% SIf% KApp% KIf% AstEnv% TVar% ζ% κ% InEval%
          Lst snoc id? prim? stx->datum)

 (only-in "../../interp-set/core/units.rkt" expand/red@)
 (only-in "../../interp-set/core/eval.rkt" [--> set:-->])
 (only-in "../core.rkt" eval/red@ [==> abs:==>] main-minus@)
 (only-in "parse.rkt" parse@)
 (only-in "domain.rkt"
          domain@ val-⊤ atom-⊤ num-⊤ sym-⊤ stx-⊤ list-⊤))
(provide ev:red@ interp)

;; Revise --> to interpret abstract values (val-⊤, stx-⊤, etc.)
;; --> : State -> (Setof State)
(define-reduction (--> delta) #:super (set:--> delta)
  #:within-signatures [(only env^
                             extend-env lookup-env)
                       (only store^
                             update-store* lookup-store alloc-loc*)
                       (only cont^
                             push-cont)]
  ;; β (val-⊤ ...)
  [`(,(SApp _lbl (cons f _) '()) ,cont ,store)
   #:when (equal? f val-⊤)
   `(,f ,cont ,store)
   ev-β-abs]

  ;; (if ⊤ ...)
  [`(,(SIf _lbl v _ tm_else) ,cont ,store)
   #:when (or (equal? v val-⊤)
              (equal? v atom-⊤))
   `(,tm_else ,cont ,store)
   ev-if-abs-#f])

(define-unit-from-reduction ev:red@ -->)

(define-mixed-unit parser@
  (import)
  (export parser^)
  (inherit [parse@ [super:parse parse] parse*])

  (define parse (super:parse super:parse parse*))
  (define parser parse))

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

  [(InEval (list stx_exp '• store_0)
           (ζ (Stxξ (Stx #f scps) ξ) '∘ κ Σ))
   #:when (or (equal? stx_exp val-⊤)
              (equal? stx_exp atom-⊤)
              (equal? stx_exp stx-⊤))
   (ζ (Stxξ stx_exp ξ) '∘ κ Σ)
   ex-macapp-flip-abs]

  ;; abstract value
  [(ζ (Stxξ val ξ) '∘ κ0 Σ)
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

(define interp (interpreter 'naive:core run delta α ≤a #f))

;; run example
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core) mode α ≤a)
  (run-suite run delta (suite 'finite) mode α ≤a))
