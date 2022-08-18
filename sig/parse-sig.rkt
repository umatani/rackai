#lang racket/signature

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

parse  ; Stx Σ -> Ast (core), Ph Stx Σ -> Ast (phases, full)
parse* ; Stl Σ -> (Listof Ast) (core), Ph Stl Σ -> (Listof Ast) (phses, full)
