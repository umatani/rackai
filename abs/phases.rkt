#lang racket
(require
 "../interpreter.rkt"
 ;"../test/suites.rkt"
 (only-in "../mix.rkt"                  define-mixed-unit)
 "../reduction.rkt"
 "../signatures.rkt"
 "../conc/phases/terms.rkt"
 (only-in "../units.rkt"                io@)
 (only-in "../conc/units.rkt"           cont@ mcont@)
 (only-in "../conc/phases/units.rkt"    debug@ [syntax@ super:syntax@]
                                        expander@)
 (only-in "../mult/units.rkt"           domain@ env@ menv@ run@)
 (only-in "../mult/core/units.rkt"      ev:red@)
 (only-in "../mult/phases/units.rkt"    parser@ expand/red@)
 (only-in "../mult/phases/expander.rkt" [==> mult:==>])
 (only-in "alloc.rkt" store@ mstore@ syntax::fin-alloc@ bind@)
 (only-in "core.rkt"  eval/red@))
(provide syntax@ ==> main-minus@ interp)


(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ empty-ctx zip unzip in-hole in-hole-stl
                          addremove strip subtract union add add-stl
                          at-phase update-ctx prune
                          flip flip-stl]
           [syntax::fin-alloc@ biggest-subset binding-lookup]))


;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (mult:==> -->)
  #:within-signatures [(only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
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

(define-unit-from-reduction ex:red@ ==>)

;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export syntax^ env^ store^ cont^ menv^ mstore^ bind^ mcont^
          run^ debug^)
  (link   syntax@ env@ store@ cont@ menv@ mstore@ bind@ mcont@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         (() eval/red@ ev)   (([ev : red^]) ev:red@)
         parser@
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export domain^ run^ debug^))


(define interp (interpreter 'abs:phases run delta α ≤a #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))

;; run example
#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a))

(module+ test1
  (process '(let ([z 1])
              ((let-syntax ([x (lambda (stx) #'z)])
                 (lambda (z) (x))) 2)))

  (process '(let ([z 1])
              ((let-syntax ([x (lambda (stx) #'z)])
                 (lambda (z) z)) 2))))

(module+ test2
  (process '((lambda (f x) (f x))
             (lambda (x) x)
             100)))
