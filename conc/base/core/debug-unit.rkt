#lang racket/unit
(require
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "terms.rkt" terms^ #%term-forms)
 (only-in "../../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ expand^ io^ run^ debug^))

(import (only terms^
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
(define (eval--> form)
  (results (do ast <- (lift (run form 'parse))
               (lift (--> `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* form #:steps [steps #f])
  (results (do ast <- (lift (run form 'parse))
               (lift (apply-reduction-relation*
                      --> `(,(AstEnv ast (init-env)) • ,(init-store))
                      #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> form)
  (==> (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Σ))
   #:steps steps))
