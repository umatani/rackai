#lang racket
(require (except-in "../core/struct.rkt"
                    Stxξ Stxξ? Stxξ-stx Stxξ-ξ))
(provide (all-from-out "../core/struct.rkt")
         (all-defined-out))

;; updated (ph scps
(struct Stxξ (ph stx ξ scps) #:transparent)
