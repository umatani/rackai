#lang racket/signature
(require
 (only-in "bind-sig.rkt" bind^))

;; ----------------------------------------
;; Expand-time store operations:

init-Σ ; -> Σ
lookup-Σ ; Σ Nam -> (U (Setof StoBind) Val ξ)

(open bind^)

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

alloc-name ; Id Σ -> (Values Nam Σ)
alloc-scope ; Symbol Σ -> (Values Scp Σ)
