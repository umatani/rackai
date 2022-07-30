#lang racket/signature

;; ----------------------------------------
;; Syntax-object operations:

empty-ctx

zip ; ProperStl ProperStl Ctx -> ProperStl
unzip ; ProperStl -> (Values ProperStl ProperStl)

in-hole ; Stl Stx -> Stl
in-hole-stl ; Stl Stx -> Stl

alloc-scope ; Symbol -> Scp

;; Adds or cancels a scope
addremove ; Scp Scps -> Scps

;; Recursively strips lexical context from a syntax object
strip ; Stl -> Val

subtract ; Scps Scps -> Scps

union ; Scps Scps -> Scps

binding-lookup ; (Setof StoBind) Scps -> (Option Nam)
biggest-subset ; Scps (Listof Scps) -> Scps

;; Simply pushes scopes down through a syntax object
add ; Stx Scp -> Stx
add-stl ; Stl Scp -> Stl

;; Pushes flipping a scope down through a syntax object
flip ; Stx Scp -> Stx
flip-stl ; Stl Scp -> Stl

at-phase ; Ctx Ph -> Scps

;; Updates the mapping of a `ctx` at a particular phase
update-ctx ; Ctx Ph Scps -> Ctx

;; Recursively removes a set of scopes from a syntax object at a given phase
prune ; Ph Stx Scps -> Stx
