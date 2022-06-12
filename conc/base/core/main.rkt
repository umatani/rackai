#lang racket
(require "../reduction.rkt" "../example.rkt"

         "../struct-sig.rkt" "struct-unit.rkt"
         "../syntax-sig.rkt" "syntax-unit.rkt"
         "../env-sig.rkt"    "env-unit.rkt"
         "../store-sig.rkt"  "store-unit.rkt"
         "../cont-sig.rkt"   "cont-unit.rkt"
         "../delta.rkt"
         "../eval-sig.rkt"   "eval-unit.rkt"
         "../menv-sig.rkt"   "menv-unit.rkt"
         "../mstore-sig.rkt" "mstore-unit.rkt"
         "../mcont-sig.rkt"  "mcont-unit.rkt"
         "../parse-sig.rkt"  "parse-unit.rkt"
         "../expand-sig.rkt" "expand-unit.rkt"
         "../io.rkt"
         "../run.rkt"

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
           (([sto  : store^])  store@      str)
           (([c    : cont^])   cont@                 sto)
           (([d    : delta^])  delta@      str)
           (([evr  : red^])    eval-red@   str     e sto c)
           (([ev   : eval^])   eval@       str     e sto d evr)
           (([me   : menv^])   menv@)
           (([msto : mstore^]) mstore@     str stx            me)
           (([mc   : mcont^])  mcont@      str       sto)
           (([p    : parse^])  parse@      str stx               msto)
           (([exr  : red^])    expand-red@ str stx e sto      me msto mc p)
           (([ex   : expand^]) expand@     str stx   sto   ev me msto mc p exr)
           (([io   : io^])     io@         str stx)
           (([r    : run^])    run@                        ev p ex io)))))
  (import)
  (export main^))

;; run example
(define (main [mode 'check])
  (run-examples run core:examples mode))

;; for debug

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
