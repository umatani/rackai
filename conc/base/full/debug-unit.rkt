#lang racket/unit
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 (only-in "../../../term.rkt" use-terms)

 (only-in "terms.rkt" terms^ #%term-forms)
 (only-in "../../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ mcont^ expand^ io^ run^ debug^))

(import (only terms^
              AstEnv% Stxξ% ζ% Σ*%)
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
        (only mcont^
              init-Θ)
        (only expand^
              ==>)
        (only io^
              reader)
        (only run^
              run))
(export debug^)

(use-terms AstEnv Stxξ ζ Σ*)

(define -->f (-->))
(define ==>f (==>))

; eval--> : Sexp -> (Setof State)
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                  #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> form)
  (==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
