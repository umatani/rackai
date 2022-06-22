#lang racket
(require "../../../reduction.rkt"
         "../../../example.rkt"
         "../../../dprint.rkt"
         
         ;;;; signatures
         (only-in "../../../struct-common-sig.rkt"      struct-common^)
         (only-in "../../../struct-common-stxe-sig.rkt" struct-common-stxe^)
         (only-in "../../base/core/struct-stxe-sig.rkt" struct-stxe^)
         (only-in "../../../syntax-sig.rkt"             syntax^)
         (only-in "../../../env-sig.rkt"                env^)
         (only-in "../../../store-sig.rkt"              store^)
         (only-in "../../../cont-sig.rkt"               cont^)
         (only-in "../../../delta-sig.rkt"              delta^)
         (only-in "../../../eval-sig.rkt"               eval^)
         (only-in "../../../menv-sig.rkt"               menv^)
         (only-in "../../../mstore-sig.rkt"             mstore^)
         (only-in "../../../mcont-sig.rkt"              mcont^)
         (only-in "../../../parse-sig.rkt"              parse^)
         (only-in "../../../expand-sig.rkt"             expand^)
         (only-in "../../../io-sig.rkt"                 io^)
         (only-in "../../../run-sig.rkt"                run^)
         
         ;;;; units

         ;; common
         (only-in "../../../struct-common-unit.rkt"   struct-common@)
         ;; common in conc/base+set-heap
         (only-in "../run-unit.rkt"                   run@)

         ;; reused from conc/base/core
         (only-in "../../base/core/struct-stxe-unit.rkt" struct-stxe@)
         (only-in "../../base/core/syntax-unit.rkt"      syntax@)
         (only-in "../../base/core/env-unit.rkt"         env@)
         (only-in "../../base/core/cont-unit.rkt"        cont@)
         (only-in "../../base/core/menv-unit.rkt"        menv@)
         (only-in "../../base/core/mcont-unit.rkt"       mcont@)
         (only-in "../../base/core/delta-unit.rkt"       delta@)
         (only-in "../../base/core/io-unit.rkt"          io@)
         ;; partialy reused from conc/base/core
         (rename-in "../../base/core/store-unit.rkt"  [store@ base:store@])
         (rename-in "../../base/core/mstore-unit.rkt" [mstore@ base:mstore@])
         ;; overridden (with set-based version)
         (only-in "store-unit.rkt"                    store@)
         (only-in "eval-unit.rkt"                     eval-red@ eval@)
         (only-in "mstore-unit.rkt"                   mstore@)
         (only-in "parse-unit.rkt"                    parse@)
         (only-in "expand-unit.rkt"                   expand-red@ expand@)
         
         (for-syntax racket/list))

(define-signature main^
  (ast&env mk-ζ       ;; struct-common^
   stx&ξ              ;; struct-stxe^
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
   ((struct-common^ struct-stxe^ env^ store^ eval^
     menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit
     (import)
     (export se sc e sto ev me msto mc ex io r)
     (link (([se   : struct-stxe^] [scse : struct-common-stxe^])
            struct-stxe@)
           (([sc   : struct-common^]) struct-common@ scse)
           (([stx  : syntax^]) syntax@     sc se)
           (([e    : env^])    env@)

           (([bsto : store^])  base:store@ sc)
           (([sto  : store^])  store@      sc bsto)

           (([c    : cont^])   cont@                 sto)
           (([d    : delta^])  delta@      sc)
           (([evr  : red^])    eval-red@   sc     e sto c)
           (([ev   : eval^])   eval@       sc     e sto d evr)
           (([me   : menv^])   menv@)

           (([bmsto : mstore^]) base:mstore@ sc stx          me)
           (([msto  : mstore^]) mstore@      sc stx          me bmsto)

           (([mc   : mcont^])  mcont@      sc       sto)
           (([p    : parse^])  parse@      sc stx               msto)
           (([exr  : red^])    expand-red@ sc se stx e sto  me msto mc p)
           (([ex   : expand^]) expand@     sc se stx sto ev me msto mc p exr)
           (([io   : io^])     io@         sc stx)
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
