#lang racket/signature

;; ----------------------------------------
;; Expand-time store operations:

init-Σ ; -> Σ

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
bind ; Σ Id Nam -> Σ

lookup-Σ ; Σ Nam -> (U (Setof StoBind) Val ξ)

resolve ; Id Σ -> Nam
id=? ; Id Nam Σ -> Boolean

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

alloc-name ; Id Σ -> (Values Nam Σ)
alloc-scope ; Symbol Σ -> (Values Scp Σ)
