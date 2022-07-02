#lang racket
(require
 (only-in "../../../reduction.rkt" red^ do <- lift apply-reduction-relation*)
 (only-in "../../../term.rkt" use-terms)
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../terms-extra.rkt" terms-extra^)
 (only-in "terms.rkt"                terms^)
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
 (only-in "../../../phase-sig.rkt"   phase^)  ;; not used
 (only-in "../../../io-sig.rkt"      io^)
 (only-in "../../../run-sig.rkt"     run^)

 ;;;; Units
 ;; common in conc/base
 (only-in "../../../terms-extra.rkt" terms-extra@)
 (only-in "../run-unit.rkt"          run@)

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
 (only-in "io-unit.rkt"              io@)

 (for-syntax racket/list))
(provide run)

(define-unit dummy-phase@
  (import)
  (export phase^)

  (define (at-phase . args)   (error "must not be used"))
  (define (prune . args)      (error "must not be used"))
  (define (update-ctx . args) (error "must not be used")))


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
    (compound-unit
     (import)
     (export t e sto ev me msto mc ex io r)
     (link
      (([t : terms^])     terms@)
      (([te : terms-extra^]) terms-extra@ t)
      (([stx  : syntax^]) syntax@     t te)
      (([e    : env^])    env@)
      (([sto  : store^])  store@      t)
      (([c    : cont^])   cont@       sto)
      (([d    : delta^])  delta@      t te)
      (([evr  : red^])    eval-red@   t te e sto c)
      (([ev   : eval^])   eval@       t te e sto d evr)
      (([me   : menv^])   menv@)
      (([msto : mstore^]) mstore@     t stx me ph)
      (([mc   : mcont^])  mcont@      t)
      (([p    : parser^]) parser@     t te stx me msto)
      (([exr  : red^])    expand-red@ t te stx e sto me msto mc p)
      (([ex   : expand^]) expand@     t stx sto ev me msto mc p exr)
      (([ph   : phase^])  dummy-phase@)
      (([io   : io^])     io@         t te stx)
      (([r    : run^])    run@        ev p ex io)))))
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
