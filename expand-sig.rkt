#lang racket/signature

;; ----------------------------------------
;; The expander:

==>      ; ζ -> (Setof ζ)
expander ; Stx -> (Cons Stx Σ)
