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

; eval--> : Sexp -> (Setof State)
(define (eval--> δ form)
  (define -->d (--> δ))
  (results (do ast <- (lift (run δ form 'parse))
               (lift ((-->d)
                      `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                        • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* δ form #:steps [steps #f])
  (define -->d (--> δ))
  (results (do ast <- (lift (run δ form 'parse))
               (lift (apply-reduction-relation*
                      (-->d) `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                               • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                      #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> δ form)
  (define ==>d (==> δ))
  ((==>d)
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (Σ* (init-Σ) (set) (set)))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* δ form #:steps [steps #f] #:compact [compact #t])
  (define ==>δ (==> δ))
  (let ([results (apply-reduction-relation*
                  (==>δ)
                  (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '•
                      (Σ* (init-Σ) (set) (set)))
                  #:steps steps)])
    (if compact
        (match results
          [(set (ζ stx ex? _ _) ...)
           (map cons (map (compose1 lst->list/recur stx->datum) stx) ex?)])
        results)))
