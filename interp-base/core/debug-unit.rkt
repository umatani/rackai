#lang racket/unit
(require
 (for-syntax racket)
 "../../reduction.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ expand^ io^ run^ debug^)
 (only-in "../../terms.rkt" [#%term-forms tm:#%term-forms]
          Stxξ%)
 (only-in "config.rkt"
          config^ [#%term-forms cfg:#%term-forms]))

(import
 (only config^
       AstEnv% ζ%)
 (only env^
       init-env)
 (only store^
       init-store)
 (only eval^
       -->)
 (only menv^
       init-ξ)
 (only mstore^
       init-Σ)
 (only expand^
       ==>)
 (only io^
       reader)
 (only run^
       run))
(export debug^)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))
(use-terms AstEnv Stxξ ζ)

; eval--> : Sexp -> (Setof State)
(define (eval--> delta form)
  (results (do ast <- (lift (run delta form 'parse))
               (lift ((--> delta)
                      `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* delta form #:steps [steps #f])
  (results (do ast <- (lift (run delta form 'parse))
               (lift (apply-reduction-relation*
                      (--> delta) `(,(AstEnv ast (init-env)) • ,(init-store))
                      #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> delta form)
  ((==> delta) (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* delta form #:steps [steps #f])
  (apply-reduction-relation*
   (==> delta)
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Σ))
   #:steps steps))
