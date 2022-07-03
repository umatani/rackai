#lang racket
(require
 "../../../reduction.rkt"
 "../../../example.rkt"
 (only-in "../../../term.rkt" use-terms)
 
 ;;;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ io^ run^)
 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)
 
 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" run@)
 (only-in "../../base/core/units.rkt"
          terms@ syntax@ env@ cont@ menv@ mcont@ delta@ io@)
 (only-in "units.rkt"
          store@ eval-red@ eval@ mstore@ parser@ expand-red@ expand@))
(provide run)

(define-signature main^
  (AstEnv% ζ% Stxξ% ;; terms^
   init-env         ;; env^
   init-store       ;; store^
   -->              ;; eval^
   init-ξ           ;; menv^
   init-Σ           ;; mstore^
   init-Θ           ;; mcont^
   ==>              ;; expand^
   reader           ;; io^
   run              ;; run^
   ))

(define-values/invoke-unit
  (unit/new-import-export
   (import) (export main^)
   ((terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit/infer
     (import)
     (export terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
     (link terms@ terms-extra@ syntax@ env@ store@ cont@ delta@
           menv@ mstore@ mcont@ parser@ io@ run@
           (([evr : red^]) eval-red@)   (() eval@ evr)
           (([exr : red^]) expand-red@) (() expand@ exr)))))
  (import) (export main^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))


;; for debug

(use-terms AstEnv Stxξ ζ)

; (: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (--> `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; (: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  --> `(,(AstEnv ast (init-env)) • ,(init-store))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==> (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
