#lang racket
(require "../../../example.rkt"
         
         ;;;; signatures
         (only-in "../../../struct-common-sig.rkt"        struct-common^)
         (only-in "../../../struct-common-stxe-sig.rkt"   struct-common-stxe^)
         (only-in "../../base/phases/struct-stxe-sig.rkt" struct-stxe^)
         (only-in "../../../syntax-sig.rkt"               syntax^)
         (only-in "../../../phase-sig.rkt"                phase^)
         (only-in "../../../env-sig.rkt"                  env^)
         (only-in "../../../store-sig.rkt"                store^)
         (only-in "../../../cont-sig.rkt"                 cont^)
         (only-in "../../../delta-sig.rkt"                delta^)
         (only-in "../../../eval-sig.rkt"                 eval^)
         (only-in "../../../menv-sig.rkt"                 menv^)
         (only-in "../../../mstore-sig.rkt"               mstore^)
         (only-in "../../../mcont-sig.rkt"                mcont^)
         (only-in "../../../parse-sig.rkt"                parse^)
         (only-in "../../../expand-sig.rkt"               expand^)
         (only-in "../../../io-sig.rkt"                   io^)
         (only-in "../../../run-sig.rkt"                  run^)

         ;; common
         (only-in "../../../struct-common-unit.rkt" struct-common@)
         ;; common in conc/base+set-heap
         (only-in "../run-unit.rkt"                 run@)
         ;; reused from conc/base/phases
         (only-in "../../base/phases/struct-stxe-unit.rkt" struct-stxe@)
         (only-in "../../base/phases/syntax-unit.rkt" syntax@)
         (only-in "../../base/core/env-unit.rkt"    env@)
         (only-in "../../base/cont/cont-unit.rkt"   cont@)
         (only-in "../../base/core/delta-unit.rkt"  delta@)
         (only-in "../../base/core/menv-unit.rkt"   menv@)
         (only-in "../../base/core/mcont-unit.rkt"  mcont@)
         (only-in "../../base/core/io-unit.rkt"     io@)
         ;; reused from conc/base+set-heap/core
         (only-in "../core/store-unit.rkt"          store@)
         (only-in "../core/eval-unit.rkt"           eval-red@ eval@)
         ;; partially reused from conc/base+set-heap/core
         (rename-in "../core/mstore-unit.rkt" [mstore@ core:mstore@])
         ;; overridden
         (only-in "mstore-unit.rkt"                 mstore@)
         (only-in "parse-unit.rkt"                  parse@)
         (only-in "expand-unit.rkt"                 expand-red@ expand@)

         ;; (only-in "../../base/example.rkt" core:examples phases:examples)
         ;; (only-in "../../base/core/misc.rkt" run-examples)
         ;; (only-in "../../base/core/eval.rkt" init-env init-store)
         ;; (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         ;; "../../base/phases/struct.rkt"
         ;; (only-in "../../base/phases/main.rkt"
         ;;          reader printer expander/expand parser/parse)

         ;; ;; Set-based version
         ;; (only-in "../core/misc.rkt" define-runner)
         ;; (only-in "../core/eval.rkt" -->c eval)
         ;; (only-in "parse.rkt" parse)
         ;; (only-in "expand.rkt" ==>p expand)

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
     (export sc se e sto ev me msto mc ex io r)
     (link (([se   : struct-stxe^] [scse : struct-common-stxe^]) struct-stxe@)
           (([sc   : struct-common^]) struct-common@ scse)
                      
           (([cstx : syntax^]) core:syntax@ sc)
           (([stx  : syntax^] [ph : phase^]) syntax@ se sc cstx)

           (([e    : env^])    env@)
           (([sto  : store^])  store@       sc)
           (([c    : cont^])   cont@                  sto)
           (([d    : delta^])  delta@       sc)
           (([evr  : red^])    eval-red@    sc     e  sto c)
           (([ev   : eval^])   eval@        sc     e  sto d evr)
           (([me   : menv^])   menv@)


           (([cmsto : mstore^]) core:mstore@ sc stx              me)
           (([msto  : mstore^]) mstore@      sc stx    ph        me cmsto)


           (([mc   : mcont^])  mcont@       sc              sto)
           (([p    : parse^])  parse@       sc stx              msto)
           (([exr  : red^])    expand-red@  sc se stx ph  e sto me msto mc p)
           (([ex   : expand^]) expand@      sc se stx sto ev me msto mc p exr)
           (([io   : io^])     io@          sc stx)
           (([r    : run^])    run@                      ev p ex io)))))
  (import)
  (export main^))


(define-runner run
  reader printer
  expander parser eval)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

;; for debug

; (: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->c `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; (: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->c `(,(AstEnv ast (init-env)) • ,(init-store))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==>p (ζ (Stxξ 0 (reader form) (init-ξ) (set))
            '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>p
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
