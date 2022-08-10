#lang racket
(require (except-in "struct-core.rkt"
                    AstEnv AstEnv? AstEnv-ast AstEnv-env
                    Stxξ Stxξ? Stxξ-stx Stxξ-ξ
                    KApp KApp? KApp-vals KApp-tms KApp-loc
                    SApp SApp? SApp-vals SApp-tms
                    κ κ? κ-stx κ-ex? κ-𝓁
                    ζ ζ? ζ-stx ζ-ex? ζ-κ ζ-Θ ζ-Σ))
(provide (all-from-out "struct-core.rkt")
         (all-defined-out))

;; add ph, maybe-scp, ξ
(struct AstEnv (ph ast env maybe-scp ξ) #:transparent)

;; remove scps from those of phases
(struct Stxξ (ph stx ξ) #:transparent)

;; new
(struct Σ* (Σ scps_p scps_u) #:transparent)

;; add ctx (List Ph MaybeScp ξ)
(struct KApp (ctx vals tms loc) #:transparent)

;; add ctx (List of Ph MaaybeScp ξ)
(struct SApp (ctx vals tms) #:transparent)

(struct InExpand (ζ state) #:transparent)

;; add Σ*
(struct κ (stx ex? Σ* 𝓁) #:transparent)

;; Σ -> Σ*
(struct ζ (stx ex? κ Θ Σ*) #:transparent)
