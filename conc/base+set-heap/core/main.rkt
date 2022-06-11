#lang racket
(require "../../base/reduction.rkt"
         "../../base/example.rkt"
         
         "../../base/struct-sig.rkt" "../../base/core/struct-unit.rkt"
         "../../base/syntax-sig.rkt"
         "../../base/io.rkt"
         "../../base/delta.rkt"
         "../../base/parse-sig.rkt"
         "../../base/eval-sig.rkt" "../../base/core/eval-unit.rkt"
         "../../base/expand-sig.rkt" "../../base/core/expand-unit.rkt"

         ;; extended
         (prefix-in base: (only-in "../../base/core/syntax-unit.rkt" syntax@))
         (only-in "syntax-unit.rkt" syntax@)
         (only-in "parse-unit.rkt" parse@)
         (only-in "../../base/run.rkt" run^)
         (only-in "../run.rkt" run@)


         ;; (only-in "../../base/core/eval.rkt" init-env init-store)
         ;; (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         ;; (only-in "../../base/core/main.rkt" expander/expand)
         
         ;; Set-based version
         ;; (only-in "eval.rkt" -->c eval)
         ;; (only-in "parse.rkt" parse)
         ;; (only-in "expand.rkt" ==>c expand)
         
         (for-syntax racket/list))

(define-signature main^
  (ast&env mk-ζ stx&ξ              ;; struct^
   empty-ctx strip                 ;; syntax^
   init-env init-store --> eval    ;; eval^
   init-ξ init-Θ init-Σ ==> expand ;; expand^
   reader printer                  ;; io^
   run                             ;; run^
   ))

(define-values/invoke-unit
  (unit/new-import-export
   (import)
   (export main^)
   ((struct^ syntax^ io^ eval^ expand^ run^)
    (compound-unit
     (import)
     (export str stx io p ev ex r)
     (link (([str  : struct^]) struct@)

           ;; extended
           (([bstx : syntax^]) base:syntax@ str)
           (([stx  : syntax^]) syntax@      str bstx)

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
