#lang typed/racket
(provide (all-defined-out))

(include "../types.rktl")

;; same as phases
(define-type Ph Integer)
(define-type Ctx (Listof (Pairof Ph Scps))) ;(HashTable Ph Scps)


;; remove scps from those of phases
(struct Stxξ ([ph : Ph] [stx : Stx] [ξ : ξ]) #:transparent)

;; new
(define-type MaybeScp (U Scp 'no-scope))

;; new
(struct Σ* ([Σ : Σ] [scps_p : Scps] [scps_u : Scps]) #:transparent)

;; add ctx (List Ph Maybe-scp ξ)
(struct KApp ([ctx : (List Ph MaybeScp ξ)]
              [vals : (Listof Val)]
              [clos : (Listof Tm)]
              [loc : Loc]) #:transparent)

;; add ph, maybe-scp, ξ
(struct AstEnv ([ph : Ph] [ast : Ast] [env : Env]
                          [maybe-scp : MaybeScp] [ξ : ξ]) #:transparent)
;; add ph, maybe-scp, ξ
(struct SApp ([ctx : (List Ph MaybeScp ξ)]
              [vals : (Listof Val)]
              [tms : (Listof Tm)]) #:transparent)

;; add Σ*, InExpand
(define-type State (U (List Tm Cont Store Σ*) InExpand))
(struct InExpand ([ζ : ζ] [state : State]) #:transparent)

;; add Σ*
(struct Mk-κ ([stx : Stx] [ex? : Ex?] [Σ* : Σ*] [𝓁 : 𝓁]) #:transparent)
;; Σ -> Σ*
(struct Zeta ([stx : Stx] [ex? : Ex?] [κ : κ] [Θ : Θ] [Σ* : Σ*])
  #:transparent)
