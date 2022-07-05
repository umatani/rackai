#lang racket/signature
(require
 (only-in "bind-sig.rkt" bind^))

;; ----------------------------------------
;; Expand-time store operations:

init-Σ ; -> Σ
lookup-Σ ; Σ Nam -> (U (Setof StoBind) Val ξ κ)
update-Σ ; Σ Nam (U (Setof StoBind) Val ξ) -> Σ

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

alloc-name  ; Id Σ -> (Values Nam Σ)
alloc-scope ; Symbol Σ -> (Values Scp Σ)
alloc-𝓁     ; Σ -> (Values 𝓁 Σ)
