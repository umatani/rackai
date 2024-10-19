#lang racket/unit
(require
 (only-in racket/match          match)
 (only-in "../../set.rkt"       set list→set)
 (only-in "../../reduction.rkt" results do <- lift apply-reduction*)
 (only-in "../../syntax.rkt"    stx->datum)
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


;; expand==> : δ Sexp → (Setof ζ)
(define (expand==> δ form)
  ((==> δ) (ζ (Stxξ (reader form) (init-ξ)) '◯ '● (init-Σ))))

;; expand==>* : δ Sexp → (Setof ζ)
(define (expand==>* δ form #:steps [steps #f] #:compact [compact #t])
  (let ([results (apply-reduction*
                  (==> δ)
                  (ζ (Stxξ (reader form) (init-ξ)) '◯ '● (init-Σ))
                  #:steps steps)])
    (if compact
        (match results
          [(set (ζ stx ex? _ _) ...)
           (list→set
            (map cons (map (compose1 lst->list/recur stx->datum) stx) ex?))])
        results)))

;; eval--> : δ Sexp → (Setof State)
(define (eval--> δ form)
  (results (do ast <- (lift (run δ form 'parse))
               (lift ((--> δ)
                      `(,(AstEnv ast (init-env)) ● ,(init-store)))))))

;; eval-->* : δ Sexp → (Setof State)
(define (eval-->* δ form #:steps [steps #f])
  (results (do ast <- (lift (run δ form 'parse))
               (lift (apply-reduction*
                      (--> δ) `(,(AstEnv ast (init-env)) ● ,(init-store))
                      #:steps steps)))))
