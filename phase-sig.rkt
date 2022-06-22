#lang racket/signature

; at-phase : Ctx Ph -> Scps
at-phase

;; Updates the mapping of a `ctx` at a particular phase
; update-ctx : Ctx Ph Scps -> Ctx
update-ctx

;; Recursively removes a set of scopes from a syntax object at a given phase
; prune : Ph Stx Scps -> Stx
prune
