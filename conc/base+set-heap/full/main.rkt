#lang racket
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ io^ run^)
 (only-in "../../base/full/terms.rkt" terms^ #%term-forms)

 ;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" run@)
 (only-in "../../base/core/units.rkt"
          env@ cont@ delta@ menv@ mcont@ io@)
 (only-in "../../base/full/units.rkt" terms@ syntax@)
 (only-in "../core/units.rkt" store@)
 (only-in "units.rkt" eval-red@ eval@ mstore@ parser@ expand-red@ expand@))
(provide run)

(define-signature main^
  (AstEnv% Stxξ% Σ*% ζ% ;; terms^
   init-env             ;; env^
   init-store           ;; store^
   -->                  ;; eval^
   init-ξ               ;; menv^
   init-Σ               ;; mstore^
   init-Θ               ;; mcont^
   ==>                  ;; expand^
   reader               ;; io^
   run                  ;; run^
   ))

(define-values/invoke-unit
  (unit/new-import-export
   (import)
   (export main^)
   ((terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit/infer
     (import)
     (export terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
     (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@
           menv@ mstore@ mcont@ parser@ io@ run@
           (([evr : red^]) eval-red@)   (() eval@ evr)
           (([exr : red^]) expand-red@) (() expand@ exr)))))
  (import) (export main^))


(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run (append local:examples defs:examples) mode))


;; for debug

(use-terms AstEnv Stxξ Σ* ζ)

(define -->f (-->))
(define ==>f (==>))

; eval--> : Sexp -> (Setof State)
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                  #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> form)
  (==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
