#lang racket
(require
 "../interpreter.rkt"
 "../test/suites.rkt"
 (only-in "../mix.rkt"                define-mixed-unit)
 (only-in "../misc.rkt"               union)
 "../reduction.rkt"
 "../signatures.rkt"
 "../base/phases/terms.rkt"

 (only-in "../mult/phases/units.rkt"
          io@ cont@ mcont@ debug@ syntax@ expander@ domain@ env@ menv@ run@
          parse@ parser@ expand/red@
          [bind@ mult:bind@] id@)
 (only-in "../mult/core/units.rkt"    ev:red@)
 (only-in "../mult/phases/expand.rkt" [==> mult:==>])
 (only-in "alloc.rkt"                 store@ mstore@
                                      biggest-subset binding-lookup)
 (only-in "core.rkt"                  eval/red@))
(provide bind@ syntax@ ==> main-minus@ interp)

(define-mixed-unit bind@
  (import  (only syntax^    at-phase)
           (only mstore^    lookup-Σ))
  (export  bind^)
  (inherit [mult:bind@      bind])

  ; resolve : Ph Id Σ -> (SetM Nam)
  (define (resolve ph id Σ0)
    (match-let ([(Stx (Sym nam) ctx) id])
      ;(printf "resolve: ~a\n" nam)
      (let* ([sbss (filter set? (set->list (results (lookup-Σ Σ0 nam))))]
             ;[_ (printf "sbss: ~a\n" sbss)]
             [scpsss
              (let ([scpsss (map (λ (sbs)
                                   (set-map sbs (λ (sb) (StoBind-scps sb))))
                                 sbss)])
                (map remove-duplicates scpsss))]
             ;[_ (printf "scpsss: ~a\n" scpsss)]
             [scps_biggests (remove-duplicates
                             (append-map (λ (scpss)
                                           (biggest-subset
                                            (at-phase ctx ph)
                                            scpss))
                                         scpsss))]
             ;[_ (printf "scps_biggests: ~a\n" scps_biggests)]
             [nam_biggests
              (remove-duplicates
               (apply append
                      (for*/list ([sbs (in-list sbss)]
                                  [scps_biggest (in-list scps_biggests)])
                        (binding-lookup sbs scps_biggest))))])
        ;(printf "nam_biggests: ~a\n" nam_biggests)
        (let ([r (if (null? nam_biggests)
                   (set nam)
                   (list->set nam_biggests))])
          ;(printf "resolve done: ~a\n" r)
          (lift r))))))


;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (mult:==> -->)
  #:within-signatures [(only syntax^
                             empty-ctx zip unzip add flip in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only  bind^    bind resolve)
                       (only    id^    id=?)
                       (only mcont^    push-κ)
                       (only parse^    parse)]
  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '∘ κ0 Σ)
   #:with nam <- (resolve ph id Σ)
   #:with  at <- (lookup-ξ ξ nam)
   (match at
     [(TVar id_new) (ζ id_new '• κ0 Σ)]
     [_ (error '==>p "unbound identifier: ~a" nam)])
   ex-var])

(define-unit-from-reduction ex:red@ ==>)

;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export syntax^ env^ store^ cont^ menv^ mstore^ bind^ id^ mcont^
          run^ debug^)
  (link   syntax@ env@ store@ cont@ menv@ mstore@ bind@ id@ mcont@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         (() eval/red@ ev)   (([ev : red^]) ev:red@)
         parse@ parser@
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp))


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
