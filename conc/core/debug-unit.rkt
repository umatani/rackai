#lang racket/unit
(require
 (only-in racket/match          match)
 (only-in "../../set.rkt"       set)
 (only-in "../../reduction.rkt" results do <- lift apply-reduction-relation*)
 "../../signatures.rkt"
 "terms.rkt")

(import (only env^       init-env)
        (only store^     init-store)
        (only eval^      -->)
        (only menv^      init-ξ)
        (only mstore^    init-Σ)
        (only expand^    ==>)
        (only io^        reader)
        (only run^       run))
(export debug^)


; eval--> : Sexp → (Setof State)
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
(define (expand==>* delta form #:steps [steps #f] #:compact [compact #t])
  (let ([results (apply-reduction-relation*
                  (==> delta)
                  (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Σ))
                  #:steps steps)])
    (if compact
        (match results
          [(set (ζ stx ex? _ _) ...)
           (map cons (map (compose1 lst->list/recur stx->datum) stx) ex?)])
        results)))
