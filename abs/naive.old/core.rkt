#lang racket
(require "../../conc/base/set.rkt"
         "../../conc/base/reduction.rkt"

         ;; toriaezu
         (only-in "../../conc/base/core/delta.rkt" delta)

         (only-in "../../conc/base/example.rkt" core:examples)
         (only-in "../../conc/base/core/misc.rkt" run-examples)
         (only-in "../../conc/base/core/eval.rkt"
                  init-env lookup-env update-env init-store)
         (only-in "../../conc/base/core/expand.rkt" init-ξ init-Θ init-Σ)
         (only-in "../../conc/base/core/main.rkt"
                  reader [printer base:printer] expander/expand)

         ;; Set-based Heap version
         (only-in "../../conc/base+set-heap/core/misc.rkt"
                  define-runner)
         (only-in "../../conc/base+set-heap/core/parse.rkt" parse)
         (only-in "../../conc/base+set-heap/core/expand.rkt"
                  expand/==> [==>c/Σ base+set-heap:==>c/Σ])
         (only-in "../../conc/base+set-heap/core/eval.rkt"
                  lookup-store update-store* alloc-loc* push-cont
                  [-->c/store base+set-heap:-->c/store])

         ;; Abstract version
         "struct-core.rkt"
         (only-in "delta.rkt" δ)

         (for-syntax racket/list))
(provide (all-defined-out))

(define (printer val)
  (with-handlers ([exn:misc:match?
                   (λ (e) (if (Val? val)
                               val
                               (raise e)))])
    (base:printer val)))

;; (: -->c : State -> (Setof State))
(define-parameterized-extended-reduction-relation (-->c/store delta)
  (base+set-heap:-->c/store delta))

(define -->c ((reducer-of -->c/store) #;δ delta))

; (: eval : Ast -> (Setof Val))
(define ((eval/--> -->) ast)
  (match-let ([(set `(,(? Val? val) • ,_store) ...)
               (apply-reduction-relation*
                --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
    (list->set val)))
(define evaluate (eval/--> -->c))


(define ==>c ((reducer-of base+set-heap:==>c/Σ) -->c))
(define expand (expand/==> ==>c))
(define expander (expander/expand expand))


(define-runner run
  reader printer
  expander parse evaluate)

(define (main [mode 'check])
  (run-examples run core:examples mode))

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
  (==>c (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>c
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
