#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 "../../reduction.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ expand^ io^ run^ debug^)
 (only-in "terms.rkt" #%term-forms
          Stxξ% AstEnv% ζ% Σ*%
          lst->list/recur stx->datum))

(import (only env^
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

(use-terms AstEnv Stxξ ζ Σ*)

; eval--> : Sexp -> (Setof State)
(define (eval--> delta form)
  (define -->d (--> delta))
  (results (do ast <- (lift (run delta form 'parse))
               (lift ((-->d)
                      `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                        • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

; eval-->* : Sexp -> (Setof State)
(define (eval-->* delta form #:steps [steps #f])
  (define -->d (--> delta))
  (results (do ast <- (lift (run delta form 'parse))
               (lift (apply-reduction-relation*
                      (-->d) `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                               • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                      #:steps steps)))))

; expand==> : Sexp -> (Setof ζ)
(define (expand==> delta form)
  (define ==>d (==> delta))
  ((==>d)
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (Σ* (init-Σ) (set) (set)))))

; expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ))
(define (expand==>* delta form #:steps [steps #f] #:compact [compact #t])
  (define ==>d (==> delta))
  (let ([results (apply-reduction-relation*
                  (==>d)
                  (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '•
                      (Σ* (init-Σ) (set) (set)))
                  #:steps steps)])
    (if compact
        (match results
          [(set (ζ stx ex? _ _) ...)
           (map cons (map (compose1 lst->list/recur stx->datum) stx) ex?)])
        results)))