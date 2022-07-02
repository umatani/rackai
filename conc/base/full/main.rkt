#lang racket
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "terms.rkt"                terms^)
 (only-in "../../../terms-extra.rkt" terms-extra^)
 (only-in "../../../syntax-sig.rkt"  syntax^)
 (only-in "../../../env-sig.rkt"     env^)
 (only-in "../../../store-sig.rkt"   store^)
 (only-in "../../../cont-sig.rkt"    cont^)
 (only-in "../../../delta-sig.rkt"   delta^)
 (only-in "../../../eval-sig.rkt"    eval^)
 (only-in "../../../menv-sig.rkt"    menv^)
 (only-in "../../../mstore-sig.rkt"  mstore^)
 (only-in "../../../mcont-sig.rkt"   mcont^)
 (only-in "../../../parser-sig.rkt"  parser^)
 (only-in "../../../expand-sig.rkt"  expand^)
 (only-in "../../../io-sig.rkt"      io^)
 (only-in "../../../run-sig.rkt"     run^)
 (only-in "../../../phase-sig.rkt"   phase^)

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
 (only-in "../core/menv-unit.rkt"    menv@)
 (only-in "../core/mcont-unit.rkt"   mcont@)
 (only-in "../core/io-unit.rkt"      io@)
 ;; overridden
 (only-in "terms.rkt"                terms@ #%term-forms)
 (only-in "syntax.rkt"               syntax@)
 (only-in "eval.rkt"                 eval-red@ eval@)
 (only-in "mstore.rkt"               mstore@)
 (only-in "parser.rkt"               parser@)
 (only-in "expand.rkt"               expand-red@ expand@)
 
 (for-syntax racket/list))
(provide run)


(define-signature main^
  (AstEnv% Stxξ% Σ*% ζ%  ;; terms^
   init-env              ;; env^
   init-store            ;; store^
   -->                   ;; eval^
   init-ξ                ;; menv^
   init-Σ                ;; mstore^
   init-Θ                ;; mcont^
   ==>                   ;; expand^
   reader                ;; io^
   run                   ;; run^
   ))

;; main
(define-values/invoke-unit
  (unit/new-import-export
   (import) (export main^)
   ((terms^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
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
      (([evr  : red^])    eval-red@   t te stx e sto c me msto mc p ph)
      (([ev   : eval^])   eval@       t te d e sto me msto ex evr)
      (([me   : menv^])   menv@)
      (([msto : mstore^]) mstore@     t stx me ph)
      (([mc   : mcont^])  mcont@      t sto)
      (([p    : parser^]) parser@     t te stx me msto)
      (([exr  : red^])    expand-red@ t te stx e sto me msto mc p ph)
      (([ex   : expand^]) expand@     t ev me msto mc exr)
      (([io   : io^])     io@         t te stx)
      (([r    : run^])    run@        ev p ex io)))))
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
