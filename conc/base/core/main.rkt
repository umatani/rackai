#lang racket
(require
 (only-in "../../../reduction.rkt" red^ do <- lift apply-reduction-relation*)
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ io^ run^)
 (only-in "terms.rkt" terms^)

 ;;;; Units
 ;; common in conc/base
 (only-in "../../../terms-extra.rkt" terms-extra@)
 (only-in "../run-unit.rkt"          run@)
 ;; new
 (only-in "terms.rkt"                terms@ #%term-forms)
 (only-in "syntax-unit.rkt"          syntax@)
 (only-in "env-unit.rkt"             env@)
 (only-in "store-unit.rkt"           store@)
 (only-in "cont-unit.rkt"            cont@)
 (only-in "delta-unit.rkt"           delta@)
 (only-in "eval.rkt"                 eval-red@ eval@)
 (only-in "menv-unit.rkt"            menv@)
 (only-in "mstore.rkt"               mstore@)
 (only-in "mcont-unit.rkt"           mcont@)
 (only-in "parser.rkt"               parser@)
 (only-in "expand.rkt"               expand-red@ expand@)
 (only-in "io-unit.rkt"              io@))
(provide run)

(define-signature main^
  (;ast&env mk-ζ stx&ξ ;; struct^
   AstEnv% Stxξ% ζ%   ;; terms^
   init-env           ;; env^
   init-store         ;; store^
   -->                ;; eval^
   init-ξ             ;; menv^
   init-Σ             ;; mstore^
   init-Θ             ;; mcont^
   ==>                ;; expand^
   reader             ;; io^
   run                ;; run^
   ))

(define-values/invoke-unit
  (unit/new-import-export
   (import) (export main^)
   ((terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit/infer
     (import)
     (export terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
     (link   terms@ terms-extra@ syntax@ env@ store@ cont@ delta@
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
