#lang racket
(require
 "../set.rkt"
 "../reduction.rkt"
 "../mix.rkt"
 (only-in "../term.rkt" use-terms)
 "../test/suites.rkt"

 (only-in "../signatures.rkt" syntax^ env^ store^ domain^
          menv^ mstore^ mcont^ bind^ eval^ parser^ expand^ expander^
          run^ debug^)
 (only-in "../interp-base/phases/terms.rkt" #%term-forms
          App% List% Pair% Null% Atom% Sym% Stx% Hole% Stxξ%
          AstEnv% TVar% ζ% κ% InEval%
          Lst snoc lst->list id? prim? val? proper-stl?)

 (only-in "../units.rkt"                      io@)
 (only-in "../interp-base/units.rkt"          cont@ mcont@)
 (only-in "../interp-base/phases/units.rkt"
          debug@ [syntax@ super:syntax@] expander@)
 (only-in "../interp-set/units.rkt"           env@ domain@ menv@ run@)
 (only-in "../interp-set/phases/units.rkt"    parser@ expand/red@)
 (only-in "../interp-set/phases/expander.rkt" [==> set:==>])
 (only-in "alloc.rkt" store@ mstore@ syntax::fin-alloc@ bind@)
 (only-in "core.rkt" eval@))
(provide syntax@ run delta α ≤a)


(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ empty-ctx zip unzip in-hole in-hole-stl
                          addremove strip subtract union add add-stl
                          at-phase update-ctx prune
                          flip flip-stl]
           [syntax::fin-alloc@ alloc-scope biggest-subset binding-lookup]))


;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (set:==> -->)
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

  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '∘ κ0 Σ)
   #:with nam <- (resolve #:phase ph id Σ)
   #:with  at <- (lookup-ξ ξ nam)
   (match at
     [(TVar id_new) (ζ id_new '• κ0 Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-compound-unit/infer expand@
  (import syntax^ env^ store^ eval^ menv^ mstore^ mcont^
          bind^ parser^)
  (export expand^)
  (link expand/red@ red@))


(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a))

(module+ test1
  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) (x))) 2)) 'eval)

  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) z)) 2)) 'eval))

(module+ test2
  (run delta '((lambda (f x) (f x))
               (lambda (x) x)
               100) 'eval))
