#lang racket
(require (except-in "struct-core.rkt"
                    Stxξ Stxξ? Stxξ-stx Stxξ-ξ))
(provide (all-from-out "struct-core.rkt")
         (all-defined-out))

;; updated (ph scps
(struct Stxξ (ph stx ξ scps) #:transparent)
