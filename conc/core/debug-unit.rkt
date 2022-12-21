#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 "../../reduction.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          env^ store^ eval^ menv^ mstore^ expand^ io^ run^ debug^)
 (only-in "terms.rkt" #%term-forms
          Stxξ% AstEnv% ζ%
          lst->list/recur stx->datum))

(import
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
