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
    
    (do `(,val ,cnt ,_sto) <- (apply-reduction*
                               -->δ `(,(c:AstEnv ast (init-env))
                                      ● ,(init-store)))
        (when (not (val? val))
          (printf "eval: non value: ~a\n" val))
        ;; possible in abs
        ;; (when (not (eq? cnt '●))
        ;;   (printf "eval: remaining cont: ~a\n" cnt))
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

    (do `(,val ,cnt ,_sto ,Σ̂′) <- (apply-reduction*
                                   (-->δ) `(,(f:AstEnv ph ast (init-env)
                                                       maybe-scpᵢ ξ)
                                            ● ,(init-store) ,Σ̂))
        (when (not (val? val))
          (printf "eval: non value: ~a\n" val))
        ;; possible in abs
        ;; (when (not (eq? cnt '●))
        ;;   (printf "eval: remaining cont: ~a\n" cnt))
        (pure (cons val Σ̂′))))

  ;; evaluator : δ Ast → (SetM Val)
  (define (evaluator δ ast)
    (do (cons val _Σ̂) <- (eval δ 0 ast 'no-scope (init-ξ) (Σ̂ (init-Σ) ∅ ∅))
        (pure val))))
