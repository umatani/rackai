#lang racket
(require
 "../../../reduction.rkt"
 "../../../example.rkt"

 ;;;; Signatures
 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "struct-stxe-sig.rkt"     struct-stxe^)
 (only-in "struct-sig.rkt"          struct^ fuse-struct-imports)
 (only-in "../../../syntax-sig.rkt" syntax^)
 (only-in "../../../phase-sig.rkt"  phase^)
 (only-in "../../../env-sig.rkt"    env^)
 (only-in "../../../store-sig.rkt"  store^)
 (only-in "../../../cont-sig.rkt"   cont^)
 (only-in "../../../delta-sig.rkt"  delta^)
 (only-in "../../../eval-sig.rkt"   eval^)
 (only-in "../../../menv-sig.rkt"   menv^)
 (only-in "../../../mstore-sig.rkt" mstore^)
 (only-in "../../../mcont-sig.rkt"  mcont^)
 (only-in "../../../parse-sig.rkt"  parse^)
 (only-in "../../../expand-sig.rkt" expand^)
 (only-in "../../../io-sig.rkt"     io^)
 (only-in "../../../run-sig.rkt"    run^)

 ;;;; Units
 ;; common in conc/base
 (only-in "../run-unit.rkt"         run@)
 ;; reused from conc/base/core
 (only-in "../core/env-unit.rkt"    env@)
 (only-in "../core/store-unit.rkt"  store@)
 (only-in "../core/cont-unit.rkt"   cont@)
 (only-in "../core/delta-unit.rkt"  delta@)
 (only-in "../core/eval-unit.rkt"   eval-red@ eval@)
 (only-in "../core/menv-unit.rkt"   menv@)
 (only-in "../core/mcont-unit.rkt"  mcont@)
 (only-in "../core/io-unit.rkt"     io@)
 ;; partialy reused from conc/base/core
 (only-in "../core/syntax-unit.rkt" [syntax@ core:syntax@])
 (only-in "../core/mstore-unit.rkt" [mstore@ core:mstore@])
 ;; overridden
 (only-in "struct-unit.rkt"         struct@)
 (only-in "syntax-unit.rkt"         [syntax@ this:syntax@])
 (only-in "mstore-unit.rkt"         [mstore@ this:mstore@])
 (only-in "parse-unit.rkt"          parse@)
 (only-in "expand-unit.rkt"         expand-red@ expand@)

 (for-syntax racket/list))

;; inheritance
(define-compound-unit syntax@
  (import [sc : struct-common^] [se : struct-stxe^])
  (export stx ph)
  (link (([cstx : syntax^]) core:syntax@ sc)
        (([stx : syntax^] [ph : phase^]) this:syntax@ sc se cstx)))

(define-compound-unit mstore@
  (import [sc : struct-common^] [stx : syntax^] [me : menv^] [ph : phase^])
  (export msto)
  (link (([cmsto : mstore^]) core:mstore@ sc stx me)
        (([msto  : mstore^]) this:mstore@ sc stx ph cmsto)))

;; Fix struct^
(define syntax*@ (fuse-struct-imports (import) (export syntax^ phase^) syntax@))
(define store*@ (fuse-struct-imports (import) (export store^) store@))
(define delta*@ (fuse-struct-imports (import) (export delta^) delta@))
(define eval-red*@ (fuse-struct-imports
                    (import env^ store^ cont^) (export red^) eval-red@))
(define eval*@ (fuse-struct-imports
                (import env^ store^ delta^ red^) (export eval^) eval@))
(define mstore*@ (fuse-struct-imports
                  (import syntax^ menv^ phase^) (export mstore^) mstore@))
(define mcont*@ (fuse-struct-imports (import) (export mcont^) mcont@))
(define parse*@ (fuse-struct-imports
                 (import syntax^ mstore^) (export parse^) parse@))
(define expand-red*@ (fuse-struct-imports
                      (import syntax^ phase^ env^ store^
                              menv^ mstore^ mcont^ parse^)
                      (export red^)  expand-red@))
(define expand*@ (fuse-struct-imports
                  (import eval^ menv^ mstore^ mcont^ red^)
                  (export expand^) expand@))
(define io*@ (fuse-struct-imports (import syntax^) (export io^) io@))

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

;; main
(define-values/invoke-unit
  (unit/new-import-export
   (import)
   (export main^)
   ((struct^ env^ store^ eval^
     menv^ mstore^ mcont^ expand^ io^ run^)
    (compound-unit
     (import)
     (export str e sto ev me msto mc ex io r)
     (link
      (([str : struct^]) struct@)

      ;; (([se   : struct-stxe^] [scse : struct-common-stxe^]) struct-stxe@)
      ;; (([sc   : struct-common^]) struct-common@ scse)
      (([stx  : syntax^] [ph : phase^]) syntax*@ str)
      (([e    : env^])    env@)
      (([sto  : store^])  store*@      str)
      (([c    : cont^])   cont@                  sto)
      (([d    : delta^])  delta*@      str)
      (([evr  : red^])    eval-red*@   str     e sto c)
      (([ev   : eval^])   eval*@       str     e sto d evr)
      (([me   : menv^])   menv@)
      (([msto : mstore^]) mstore*@     str stx ph me)
      (([mc   : mcont^])  mcont*@      str              sto)
      (([p    : parse^])  parse*@      str stx              msto)
      (([exr  : red^])    expand-red*@ str stx ph e sto me msto mc p)
      (([ex   : expand^]) expand*@     str stx sto ev me msto mc p exr)
      (([io   : io^])     io*@         str stx)
      (([r    : run^])    run@                      ev p ex io)))))
  (import)
  (export main^))


(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

;; for debug

; eval--> : Sexp -> (Setof State)
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (--> `(,(ast&env ast (init-env)) • ,(init-store)))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  --> `(,(ast&env ast (init-env)) • ,(init-store))
                  #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> form)
  (==> (mk-ζ (stx&ξ 0 (reader form) (init-ξ) (set))
              '∘ '• (init-Θ) (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (mk-ζ (stx&ξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
