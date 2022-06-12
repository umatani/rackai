#lang racket/signature

;; ----------------------------------------
;; The expander:

==> ; ζ -> (Setof ζ)
expand   ; Stx ξ Σ -> (Cons Stx Σ)
expander ; Stx -> (Values Stx Σ)
