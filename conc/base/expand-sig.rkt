#lang racket/signature

;; ----------------------------------------
;; The expander:

;; ----------------------------------------
;; Expand-time environment operations:

init-ξ ; -> ξ
lookup-ξ ; ξ Nam -> AllTransform
extend-ξ ; ξ Nam AllTransform -> ξ

;; ----------------------------------------
;; Expand-time stack operations:

init-Θ ; -> Θ

alloc-κ ; Θ -> (Values 𝓁 Θ)
lookup-κ ; Θ 𝓁 -> κ
update-κ ; Θ 𝓁 κ -> Θ
push-κ ; Θ κ -> (Values 𝓁 Θ)

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

init-Σ ; -> Σ

alloc-name ; Id Σ -> (Values Nam Σ)
alloc-scope ; Symbol Σ -> (Values Scp Σ)

regist-vars ; Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ)

id-kont
id-seq
id-snoc
stx-nil

;==>/Σ
==> ; ζ -> (Setof ζ)

expand/==> 
expand   ; Stx ξ Σ -> (Cons Stx Σ)
expander ; Stx -> (Values Stx Σ)
