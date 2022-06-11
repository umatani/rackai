#lang racket/signature

empty-ctx

;; ----------------------------------------
;; stx utils

stl->seq ; Stl -> (Listof Stx)
zip ; ProperStl ProperStl Ctx -> ProperStl
unzip ; ProperStl -> (Values ProperStl ProperStl)
snoc ; ProperStl Stx -> ProperStl

in-hole ; Stl Stx -> Stl
in-hole-stl ; Stl Stx -> Stl

;; Adds or cancels a scope
addremove ; Scp Scps -> Scps

;; Recursively strips lexical context from a syntax object
strip ; Stl -> Val

subtract ; Scps Scps -> Scps

union ; Scps Scps -> Scps

binding-lookup ; (Setof StoBind) Scps -> (Option Nam)
biggest-subset ; Scps (Listof Scps) -> Scps

;; ----------------------------------------
;; Syntax-object operations:

;; Simply pushes scopes down through a syntax object
add ; Stx Scp -> Stx
add-stl ; Stl Scp -> Stl

;; Pushes flipping a scope down through a syntax object
flip ; Stx Scp -> Stx
flip-stl ; Stl Scp -> Stl

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
bind ; Σ Id Nam -> Σ

lookup-Σ ; Σ Nam -> (U (Setof StoBind) Val ξ)

resolve ; Id Σ -> Nam
id=? ; Id Nam Σ -> Boolean

