#lang typed/racket
(provide (all-defined-out))

(include "../types.rktl")

(define-type Ctx Scps)

(struct Stxξ ([stx : Stx] [ξ : ξ]) #:transparent)

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
