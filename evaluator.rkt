#lang racket
(require
 (only-in "set.rkt"             ∅)
 (only-in "nondet.rkt"          do <- pure)
 (only-in "reduction.rkt"       apply-reduction*)
 "signatures.rkt"
 (only-in "terms.rkt"           [AstEnv c:AstEnv])
 (only-in "base/full/terms.rkt" [AstEnv f:AstEnv] Σ̂))
(provide core-evaluator@ full-evaluator@)

(define-unit core-evaluator@
  (import
   (only domain^    val?)
   (only    env^    init-env)
   (only  store^    init-store)
   (only   eval^    -->))
  (export evaluator^)

  ;; evaluator : δ Ast → (SetM Val)
  (define (evaluator δ ast)
    (define -->δ (--> δ))
    
    (do `(,(? val? val) ● ,_sto) <- (apply-reduction*
                                     -->δ `(,(c:AstEnv ast (init-env))
                                            ● ,(init-store)))
        (pure val))))


(define-unit full-evaluator@
  (import (only domain^    val?)
          (only    env^    init-env)
          (only  store^    init-store)
          (only   menv^    init-ξ)
          (only mstore^    init-Σ)
          (only   eval^    -->))
  (export evaluator^)

  ;; eval : Ph Ast MaybeScp ξ Σ̂ → (SetM (Cons Val Σ̂))
  (define (eval δ ph ast maybe-scpᵢ ξ Σ̂)
    (define -->δ (--> δ))

    (do `(,(? val? val) ● ,_sto ,Σ̂′) <- (apply-reduction*
                                         (-->δ) `(,(f:AstEnv ph ast (init-env)
                                                             maybe-scpᵢ ξ)
                                                  ● ,(init-store) ,Σ̂))
        (pure (cons val Σ̂′))))

  ;; evaluator : δ Ast → (SetM Val)
  (define (evaluator δ ast)
    (do (cons val _Σ̂) <- (eval δ 0 ast 'no-scope (init-ξ) (Σ̂ (init-Σ) ∅ ∅))
        (pure val))))
