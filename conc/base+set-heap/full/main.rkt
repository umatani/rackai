#lang racket
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;; Signatures
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ delta^ eval^
          menv^ mstore^ mcont^ parser^ expand^ phase^ io^ run^)
 (only-in "../../base/full/terms.rkt" terms^)

 ;; Units
 ;; common
 (only-in "../../../terms-extra.rkt"       terms-extra@)
 ;; common in conc/base+set-heap
 (only-in "../run-unit.rkt"                run@)
 ;; reused from conc/base/core
 (only-in "../../base/core/env-unit.rkt"   env@)
 (only-in "../../base/core/cont-unit.rkt"  cont@)
 (only-in "../../base/core/delta-unit.rkt" delta@)
 (only-in "../../base/core/menv-unit.rkt"  menv@)
 (only-in "../../base/core/mcont-unit.rkt" mcont@)
 (only-in "../../base/core/io-unit.rkt"    io@)
 ;; reused from conc/base/full
 (only-in "../../base/full/terms.rkt"      terms@ #%term-forms)
 (only-in "../../base/full/syntax.rkt"     syntax@)
 ;; reused from conc/base+set-heap/core
 (only-in "../core/store.rkt"              store@)
 ;; new
 (only-in "eval.rkt"                       eval-red@ eval@)
 (only-in "mstore.rkt"                     mstore@)
 (only-in "parser.rkt"                     parser@)
 (only-in "expand.rkt"                     expand-red@ expand@))
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
           (([exr : red^]) expand-red@) (() expand@ exr)))
    #;
    (compound-unit
     (import)
     (export t e sto ev me msto mc ex io r)
     (link (([t    : terms^])  terms@)
           (([te   : terms-extra^]) terms-extra@ t)
           (([stx  : syntax^]
             [ph   : phase^])  syntax@     t te)
           (([e    : env^])    env@)
           (([sto  : store^])  store@      t)
           (([c    : cont^])   cont@       sto)
           (([d    : delta^])  delta@      t te)
           (([evr  : red^])    eval-red@   t te stx e sto c me msto mc p ph)
           (([ev   : eval^])   eval@       t te e sto d me msto ex evr)
           (([me   : menv^])   menv@)
           (([msto : mstore^]) mstore@     t stx me ph)
           (([mc   : mcont^])  mcont@      t)
           (([p    : parser^]) parser@     t te stx me msto)
           (([exr  : red^])    expand-red@ t te stx e sto me msto mc p ph)
           (([ex   : expand^]) expand@     t ev me msto mc exr)
           (([io   : io^])     io@         t te stx)
           (([r    : run^])    run@        io ev p ex)))))
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
