#lang typed/racket
(provide (all-defined-out))

(include "../types.rktl")

;; same as core
(struct KApp ([vals : (Listof Val)]
              [clos : (Listof Tm)]
              [loc : Loc]) #:transparent)
(struct AstEnv ([ast : Ast] [env : Env]) #:transparent)
(struct SApp ([vals : (Listof Val)]
              [tms : (Listof Tm)]) #:transparent)
(define-type State (List Tm Cont Store))
(struct Mk-κ ([stx : Stx] [ex? : Ex?] [𝓁 : 𝓁]) #:transparent)
(struct Zeta ([stx : Stx] [ex? : Ex?] [κ : κ] [Θ : Θ] [Σ : Σ])
  #:transparent)


;; new
(define-type Ph Integer)

;; updated scps -> (Map [ph scps] ...)
(define-type Ctx (Listof (Pairof Ph Scps))) ;(HashTable Ph Scps)

;; updated (ph scps)
(struct Stxξ ([ph : Ph] [stx : Stx] [ξ : ξ] [scps : Scps]) #:transparent)

