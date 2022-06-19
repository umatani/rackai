#lang racket
(require "../../../reduction.rkt"
         "../../../example.rkt"
         "../../../dprint.rkt"
         
         "../../../struct-sig.rkt"
         "../../../syntax-sig.rkt"
         "../../../env-sig.rkt"
         "../../../store-sig.rkt"
         "../../../cont-sig.rkt"
         "../../../delta-sig.rkt"
         "../../../eval-sig.rkt"
         "../../../menv-sig.rkt"
         "../../../mstore-sig.rkt"
         "../../../mcont-sig.rkt"
         "../../../parse-sig.rkt"
         "../../../expand-sig.rkt"
         "../../../io-sig.rkt"
         "../../../run-sig.rkt"

         ;; inherited
         (only-in "../../base/core/struct-unit.rkt" struct@)
         (only-in "../../base/core/syntax-unit.rkt" syntax@)
         (only-in "../../base/core/env-unit.rkt"    env@)
         (rename-in "../../base/core/store-unit.rkt" [store@ base:store@])
         (only-in "../../base/core/cont-unit.rkt"   cont@)
         (only-in "../../base/delta-unit.rkt"       delta@)
         (only-in "../../base/core/menv-unit.rkt"   menv@)
         (rename-in "../../base/core/mstore-unit.rkt" [mstore@ base:mstore@])
         (only-in "../../base/core/mcont-unit.rkt"  mcont@)
         (only-in "../../base/io-unit.rkt"          io@)

         ;; extended
         (only-in "store-unit.rkt"  store@)
         (only-in "eval-unit.rkt"   eval-red@ eval@)
         (only-in "mstore-unit.rkt" mstore@)
         (only-in "parse-unit.rkt"  parse@)
         (only-in "expand-unit.rkt" expand-red@ expand@)
         (only-in "../run-unit.rkt" run@)


         ;; (only-in "../../base/core/eval.rkt" init-env init-store)
         ;; (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         ;; (only-in "../../base/core/main.rkt" expander/expand)
         
         ;; Set-based version
         ;; (only-in "eval.rkt" -->c eval)
         ;; (only-in "parse.rkt" parse)
         ;; (only-in "expand.rkt" ==>c expand)
         
         (for-syntax racket/list))

(define-signature main^
  (ast&env mk-ζ stx&ξ ;; struct^
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
   (import)
   (export main^)
   ((struct^ env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit
     (import)
     (export str e sto ev me msto mc ex io r)
     (link (([str  : struct^]) struct@)
           (([stx  : syntax^]) syntax@     str)
           (([e    : env^])    env@)

           (([bsto : store^])  base:store@ str)
           (([sto  : store^])  store@      str bsto)

           (([c    : cont^])   cont@                 sto)
           (([d    : delta^])  delta@      str)
           (([evr  : red^])    eval-red@   str     e sto c)
           (([ev   : eval^])   eval@       str     e sto d evr)
           (([me   : menv^])   menv@)

           (([bmsto : mstore^]) base:mstore@ str stx          me)
           (([msto  : mstore^]) mstore@      str stx          me bmsto)


           (([mc   : mcont^])  mcont@      str       sto)
           (([p    : parse^])  parse@      str stx               msto)
           (([exr  : red^])    expand-red@ str stx e sto      me msto mc p)
           (([ex   : expand^]) expand@     str stx   sto   ev me msto mc p exr)
           (([io   : io^])     io@         str stx)
           (([r    : run^])    run@                        ev p ex io)))))
  (import)
  (export main^))

#;
(define-signature main^
  (ast&env mk-ζ stx&ξ              ;; struct^
   empty-ctx strip                 ;; syntax^
   init-env init-store --> eval    ;; eval^
   init-ξ init-Θ init-Σ ==> expand ;; expand^
   reader printer                  ;; io^
   run                             ;; run^
   ))

#;
(define-values/invoke-unit
  (unit/new-import-export
   (import)
   (export main^)
   ((struct^ syntax^ io^ eval^ expand^ run^)
    (compound-unit
     (import)
     (export str stx io p ev ex r)
     (link (([str  : struct^]) struct@)
           (([stx  : syntax^]) syntax@ str bstx)
           (([io   : io^])     io@     str stx)
           (([d    : delta^])  delta@  str)
           (([p    : parse^])  parse@  str stx)
           (([ev   : eval^])   eval@   str d)
           (([ex   : expand^]) expand@ str stx p ev)
           (([r    : run^])    run@    io p ev ex)))))
  (import)
  (export main^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))

;; ;; for debug

; (: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (--> `(,(ast&env ast (init-env)) • ,(init-store)))))))

; (: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  --> `(,(ast&env ast (init-env)) • ,(init-store))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==> (mk-ζ (stx&ξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (mk-ζ (stx&ξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
