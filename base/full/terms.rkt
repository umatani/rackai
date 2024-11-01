#lang racket/base
(require
 (for-syntax racket/base)
 (only-in "../../term.rkt" define-term use-terms)
 (rename-in (except-in "../phases/terms.rkt" Stxξ AstEnv κ ζ
                       )
            [#%term-forms phases:#%term-forms]
            [Stxξ%        phases:Stxξ%]
            [AstEnv%      phases:AstEnv%]
            [κ%           phases:κ%]
            [ζ%           phases:ζ%]))
(provide (all-defined-out)
         (except-out (all-from-out "../phases/terms.rkt")
                     phases:Stxξ%
                     phases:AstEnv%
                     phases:κ%
                     phases:ζ%))

;; remove scps from those of phases
(define-term Stxξ     phases:Stxξ   () #:remove [scpsₚ])

;; add ph, maybe-scp, and ξ
(define-term AstEnv   phases:AstEnv (ph maybe-scp ξ))
;; new
(define-term Σ̂                      (Σ scpsₚ scpsᵤ))
;; new
(define-term InExpand               (ζ state))
;; add scpsₚ and scpsᵤ
(define-term κ        phases:κ      (scpsₚ scpsᵤ))
;; change Σ to Σ̂
(define-term ζ        phases:ζ      (Σ̂) #:remove [Σ])

;; used only in full
(define-term Defs     Atom          (scp 𝓁))


(define-syntax #%term-forms
  (append '((Stxξ     ph stx ξ))
          '((AstEnv   ph ast env maybe-scp ξ)
            (Σ̂        Σ scpsₚ scpsᵤ)
            (InExpand ζ state)
            (κ        stxξ scpsₚ scpsᵤ 𝓁)
            (ζ        stxξ κ Σ̂)
            (Defs     scp 𝓁))
          (syntax-local-value #'phases:#%term-forms)))

(use-terms Stxξ AstEnv Σ̂ InExpand κ ζ Defs)
