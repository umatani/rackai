#lang typed/racket
(provide (all-defined-out))

(include "../types.rktl")

; new
(define-type Ph Integer)

; updated scps -> (Map [ph scps] ...)
(define-type Ctx (Listof (Pairof Ph Scps))) ;(HashTable Ph Scps)

; updated (ph scps)
(struct Stxξ ([ph : Ph] [stx : Stx] [ξ : ξ] [scps : Scps]) #:transparent)
