#lang racket
(require (except-in "../core/struct.rkt"
                    ;; KApp KApp? KApp-tms KApp-vals KApp-loc
                    ;; SApp SApp? SApp-tms SApp-vals
                    ;; AstEnv AstEnv? AstEnv-ast AstEnv-env
                    ;; κ κ? κ-stx κ-ex? κ-𝓁
                    ;; ζ ζ? ζ-stx ζ-ex? ζ-κ ζ-Σ
                    Stxξ Stxξ? Stxξ-stx Stxξ-ξ))
(provide (all-from-out "../core/struct.rkt")
         (all-defined-out))

;; new
(define (Ph? x) (integer? x))

;; updated (ph scps
(struct Stxξ (ph stx ξ scps) #:transparent)
