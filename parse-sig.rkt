#lang racket/signature

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

parse ; Stx Σ -> Ast
parser
