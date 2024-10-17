#lang racket/unit
(require
 (only-in racket/match    match)
 (only-in "../../set.rkt" set)
 "../../reduction.rkt"
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

; eval--> : δ → Sexp -> (Setof State)
(define (eval--> δ form)
  (results (do ast <- (lift (run δ form 'parse))
               (lift ((--> δ)
                      `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* δ form #:steps [steps #f])
  (results (do ast <- (lift (run δ form 'parse))
               (lift (apply-reduction-relation*
                      (--> δ) `(,(AstEnv ast (init-env)) • ,(init-store))
                      #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> δ form)
  ((==> δ) (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Σ))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* δ form #:steps [steps #f] #:compact [compact #t])
  (let ([results (apply-reduction-relation*
                  (==> δ)
                  (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Σ))
                  #:steps steps)])
    (if compact
        (match results
          [(set (ζ stx ex? _ _) ...)
           (map cons (map (compose1 lst->list/recur stx->datum) stx) ex?)])
        results)))
