#lang racket/base
(require
 (for-syntax racket/base)
 (only-in "../../term.rkt" define-term use-terms)
 (rename-in (except-in "../phases/terms.rkt" Stxξ AstEnv KApp SApp κ ζ)
            [#%term-forms phases:#%term-forms]
            [Stxξ%        phases:Stxξ%]
            [AstEnv%      phases:AstEnv%]
            [KApp%        phases:KApp%]
            [SApp%        phases:SApp%]
            [κ%           phases:κ%]
            [ζ%           phases:ζ%]))
(provide (all-defined-out)
         (except-out (all-from-out "../phases/terms.rkt")
                     phases:Stxξ%
                     phases:AstEnv%
                     phases:KApp%
                     phases:SApp%
                     phases:κ%
                     phases:ζ%))

;; remove scps from those of phases
(define-term Stxξ     phases:Stxξ   () #:remove [scps])

;; add ph, maybe-scp, and ξ
(define-term AstEnv   phases:AstEnv (ph maybe-scp ξ))
;; new
(define-term Σ*                     (Σ scps_p scps_u))
;; add ctx (List Ph MaybeScp ξ)
(define-term KApp     phases:KApp   (ctx))
;; add ctx (List Ph MaybeScp ξ)
(define-term SApp     phases:SApp   (ctx))
;; new
(define-term InExpand               (ζ state))
;; add Σ*
(define-term κ        phases:κ      (Σ*))
;; Σ -> Σ*
(define-term ζ        phases:ζ      (Σ*) #:remove [Σ])


(define-syntax #%term-forms
  (append '((Stxξ     ph stx ξ))
          '((AstEnv   ph ast env maybe-scp ξ)
            (Σ*       Σ scps_p scps_u)
            (KApp     ctx lbl vals tms loc)
            (SApp     ctx lbl vals tms)
            (InExpand ζ state)
            (κ        stx ex? Σ* 𝓁)
            (ζ        stx ex? κ Σ*))
          (syntax-local-value #'phases:#%term-forms)))

(use-terms Stxξ AstEnv Σ* KApp SApp InExpand κ ζ)
