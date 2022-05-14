#lang typed/racket
(provide (all-defined-out))

(include "../types.rktl")

(define-type Ctx Scps)

(struct Stxξ ([stx : Stx] [ξ : ξ]) #:transparent)
