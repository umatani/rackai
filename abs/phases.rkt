#lang racket/base
(require
 racket/unit
 (only-in racket/list        remove-duplicates append-map)
 (only-in racket/match       match match-let)
 "../interpreter.rkt"
 "../signatures.rkt"
 (only-in "../reduction.rkt" define-reduction define-unit-from-reduction
                             enable-tracing)
 (only-in "../nondet.rkt"    := <- lift results)
 (only-in "../set.rkt"       set set? ∅ ∅? set-add set→list list→set set-map)
 (only-in "../mix.rkt"       define-mixed-unit inherit)
 (only-in "../syntax.rkt"    snoc)
 "../test/suites.rkt"
 "../base/phases/terms.rkt"
 (only-in "../mult/phases/units.rkt"
          common@ io@ debug@ syntax@ expander@ domain@ env@ menv@ run@
          parse@ parser@ [bind@ mult:bind@] id@)
 (only-in "../mult/phases/units.rkt"  eval@ expand@)
 (only-in "../mult/phases/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "alloc.rkt"                 store@ mstore@
                                      biggest-subset binding-lookup)
 (only-in "core.rkt"                  evaluator@))
(provide bind@ syntax@ main-minus@ interp)

;;;; bind^

(define-mixed-unit bind@
  (import  (only syntax^    at-phase)
           (only mstore^    lookup-Σ))
  (export  bind^)
  (inherit [mult:bind@      bind])

  ; resolve : Ph Id Σ -> (SetM Nam)
  (define (resolve ph id Σ0)
    (match-let ([(Stx (Sym nam) ctx) id])
      (let* ([sbss (filter set? (set→list (results (lookup-Σ Σ0 nam))))]
             [scpsss
              (let ([scpsss (map (λ (sbs)
                                   (set-map (λ (sb) (StoBind-scps sb)) sbs))
                                 sbss)])
                (map remove-duplicates scpsss))]
             [scps_biggests (remove-duplicates
                             (append-map (λ (scpss)
                                           (biggest-subset
                                            (at-phase ctx ph)
                                            scpss))
                                         scpsss))]
             [nam_biggests
              (remove-duplicates
               (apply append
                      (for*/list ([sbs (in-list sbss)]
                                  [scps_biggest (in-list scps_biggests)])
                        (binding-lookup sbs scps_biggest))))])
        (let ([r (if (null? nam_biggests)
                   (set nam)
                   (list→set nam_biggests))])
          (lift r))))))


;;;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export common^ syntax^ env^ store^ evaluator^ menv^ mstore^ bind^ id^
          run^ debug^)
  (link   common@ syntax@ env@ store@ evaluator@ menv@ mstore@ bind@ id@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link main-minus@
         domain@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp))


(module+ test1
  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) (x))) 2)))

  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) z)) 2))))

(module+ test2
  (interp '((lambda (f x) (f x))
            (lambda (x) x)
            100)))
