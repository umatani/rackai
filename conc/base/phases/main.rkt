#lang racket
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ io^ run^)
 (only-in "terms.rkt" terms^ #%term-forms)

 ;;;; Units
 (only-in "../../../units.rkt" terms-extra@)
 (only-in "../units.rkt" run@)
 (only-in "../core/units.rkt"
          env@ store@ cont@ delta@ eval-red@ eval@ menv@ mcont@ io@)
 (only-in "units.rkt"
          terms@ syntax@ mstore@ parser@ expand-red@ expand@))
(provide run)

(define-signature main^
  (AstEnv% ζ% Stxξ% ;; struct^
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

;; main
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

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

;; for debug

(use-terms AstEnv ζ Stxξ)

; eval--> : Sexp -> (Setof State)
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (--> `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  --> `(,(AstEnv ast (init-env)) • ,(init-store))
                  #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> form)
  (==> (ζ (Stxξ 0 (reader form) (init-ξ) (set))
           '∘ '• (init-Θ) (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
