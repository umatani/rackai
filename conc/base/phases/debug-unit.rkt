#lang racket/unit
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)
 (only-in "../../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ expand^ io^ run^ debug^)
 (only-in "config.rkt" config^ #%term-forms))

(import
 (only config^
       AstEnv% Stxξ% ζ%)
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
  ((==> delta) (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* delta form #:steps [steps #f])
  (apply-reduction-relation*
   (==> delta)
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Σ))
   #:steps steps))
