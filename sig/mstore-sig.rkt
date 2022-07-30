#lang racket/signature
(require
 (only-in "bind-sig.rkt" bind^))

;; ----------------------------------------
;; Expand-time store operations:

init-Σ   ; -> Σ
lookup-Σ ; Σ Nam -> (Setof StoBind)
         ; Σ 𝓁   -> (U Val ξ κ)
update-Σ ; Σ Nam (Setof StoBind) -> Σ
         ; Σ 𝓁   (U Val ξ κ)     -> Σ

;; ----------------------------------------
;; Alloc name & 𝓁 helpers for expander:

alloc-name  ; Id  Σ -> (Values Nam Σ)
alloc-𝓁     ; Stx Σ -> (Values 𝓁   Σ)

