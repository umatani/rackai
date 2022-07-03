#lang racket
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ phase^ io^ run^)
 (only-in "terms.rkt" terms^)

 ;;;; Units
 ;; common
 (only-in "../../../terms-extra.rkt" terms-extra@)
 ;; common in conc/base
 (only-in "../run-unit.rkt"          run@)
 ;; reused from conc/base/core
 (only-in "../core/env-unit.rkt"     env@)
 (only-in "../core/store-unit.rkt"   store@)
 (only-in "../core/cont-unit.rkt"    cont@)
 (only-in "../core/delta-unit.rkt"   delta@)
 (only-in "../core/eval.rkt"         eval-red@ eval@)
 (only-in "../core/menv-unit.rkt"    menv@)
 (only-in "../core/mcont-unit.rkt"   mcont@)
 (only-in "../core/io-unit.rkt"      io@)
 ;; new
 (only-in "terms.rkt"                terms@ #%term-forms)
 (only-in "syntax.rkt"               syntax@)
 (only-in "mstore.rkt"               mstore@)
 (only-in "parser.rkt"               parser@)
 (only-in "expand.rkt"               expand-red@ expand@))
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
           (([exr : red^]) expand-red@) (() expand@ exr)))
    #;
    (compound-unit
     (import)
     (export t e sto ev me msto mc ex io r)
     (link
      (([t    : terms^]) terms@)
      (([te   : terms-extra^]) terms-extra@ t)
      (([stx  : syntax^]
        [ph   : phase^])  syntax@     t te)
      (([e    : env^])    env@)
      (([sto  : store^])  store@      t)
      (([c    : cont^])   cont@       sto)
      (([d    : delta^])  delta@      t te)
      (([evr  : red^])    eval-red@   t te e sto c)
      (([ev   : eval^])   eval@       t te e sto d evr)
      (([me   : menv^])   menv@)
      (([msto : mstore^]) mstore@     t stx ph me)
      (([mc   : mcont^])  mcont@      t sto)
      (([p    : parser^]) parser@     t te stx me msto)
      (([exr  : red^])    expand-red@ t te stx ph e sto me msto mc p)
      (([ex   : expand^]) expand@     t stx sto ev me msto mc p exr)
      (([io   : io^])     io@         t te stx)
      (([r    : run^])    run@        ev p ex io)))))
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
