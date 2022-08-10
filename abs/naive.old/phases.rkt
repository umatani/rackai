#lang racket
(require "../../conc/base/reduction.rkt"
         (only-in "../../conc/base/example.rkt" core:examples phases:examples)
         (only-in "../../conc/base/core/misc.rkt" run-examples)
         (only-in "../../conc/base/core/eval.rkt" init-env init-store)
         (only-in "../../conc/base/core/expand.rkt" init-ξ init-Θ init-Σ)
         (only-in "../../conc/base/phases/main.rkt"
                  reader printer expander/expand)

         ;; Set-based Heap version
         (only-in "../../conc/base+set-heap/core/misc.rkt" define-runner)
         (only-in "../../conc/base+set-heap/phases/expand.rkt"
                  expand/==> [==>p/Σ base+set-heap:==>p/Σ])
         (only-in "../../conc/base+set-heap/phases/main.rkt"
                  parser)

         ;; Abstract version
         "struct-phases.rkt"
         (only-in "core.rkt" -->c evaluate)

         (for-syntax racket/list))
(provide (all-defined-out))

(define ==>p ((reducer-of base+set-heap:==>p/Σ) -->c))
(define expand (expand/==> ==>p))
(define expander (expander/expand expand))

(define-runner run
  reader printer
  expander parser evaluate)

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
