#lang racket
(require "../reduction.rkt" "../example.rkt"

         "../struct-sig.rkt" "struct-unit.rkt"
         "../syntax-sig.rkt" "syntax-unit.rkt"
         "../io.rkt"
         "../delta.rkt"
         "../parse-sig.rkt"  "parse-unit.rkt"
         "../eval-sig.rkt"   "eval-unit.rkt"
         "../expand-sig.rkt" "expand-unit.rkt"
         "../run.rkt"

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
     (link (([str : struct^]) struct@)
           (([stx : syntax^]) syntax@ str)
           (([io  : io^])     io@     str stx)
           (([d   : delta^])  delta@  str)
           (([p   : parse^])  parse@  str stx)
           (([ev  : eval^])   eval@   str d)
           (([ex  : expand^]) expand@ str stx p ev)
           (([r   : run^])    run@    io p ev ex)))))
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
