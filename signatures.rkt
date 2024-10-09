#lang racket
(provide (all-defined-out))


(define-signature bind^
  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  (bind    ; Ph Σ Id Nam -> Σ
   resolve
   id=?))

;; ----------------------------------------
 ;; Continuation:
(define-signature cont^
  (push-cont ; Store Cont -> (Values Loc Store)
   ))

;; for debug
(define-signature debug^
  (eval--> eval-->* expand==> expand==>*))

;; ----------------------------------------
;; Implementation of Domains:
(define-signature domain^
  (delta
   α
   ≤a

   val?
   stx?
   stl?
   proper-stl?))

;; ----------------------------------------
;; Environment:
(define-signature env^
  (init-env    ; -> Env
   lookup-env  ; Env Var -> Loc
   extend-env* ; Env (Listof Var) (Listof Loc) -> Env
   ))

;; ----------------------------------------
;; Evaluating AST:
(define-signature eval^
  (-->      ; State -> (Setof State)
   evaluate ; Ast -> Val
   ))

;; ----------------------------------------
;; The expand:
(define-signature expand^
  (==>    ; ζ -> (Setof ζ)
   expand ; Stx -> (Cons Stx Σ)
   ))

;; ----------------------------------------
;; The expander:
(define-signature expander^
  (expander))

;;;; reader & printer
(define-signature io^
  (reader
   printer))

;; ----------------------------------------
;; Expand-time stack operations:
(define-signature mcont^
  (push-κ ; Σ κ -> (Values 𝓁 Σ)
   ))

;; ----------------------------------------
;; Expand-time environment operations:
(define-signature menv^
  (init-ξ   ; -> ξ
   lookup-ξ ; ξ Nam -> AllTransform
   extend-ξ ; ξ Nam AllTransform -> ξ
   ))

(define-signature mstore^
  (;; ----------------------------------------
   ;; Expand-time store operations:
   init-Σ   ; -> Σ
   lookup-Σ ; Σ Nam -> (Setof StoBind)
   ; Σ 𝓁   -> (U Val ξ κ)
   update-Σ ; Σ Nam (Setof StoBind) -> Σ
   ; Σ 𝓁   (U Val ξ κ)     -> Σ

   ;; ----------------------------------------
   ;; Alloc name & 𝓁 helpers for expander:
   alloc-name ; Id  Σ -> (Values Nam Σ)
   alloc-𝓁    ; Stx Σ -> (Values 𝓁   Σ)
   ))

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):
(define-signature parse^
  (parse  ; Stx Σ -> Ast (core), Ph Stx Σ -> Ast (phases, full)
   parse* ; Stl Σ -> (Listof Ast) (core), Ph Stl Σ -> (Listof Ast) (phses, full)
   ))

(define-signature parser^
  (parse parser))

;;;; runner
(define-signature run^
  (run))

;; ----------------------------------------
;; Store:
(define-signature store^
  (init-store    ; -> Store
   lookup-store  ; Store Loc -> (U Val Cont)
   update-store  ; Store Loc (U Val Cont) -> Store
   update-store* ; Store (Listof Loc) (Listof (U Val Cont)) -> Store

   alloc-loc  ; Store -> (Values Loc Store)
   ;; for eval-time value binding
   alloc-loc* ; (Listof Nam) Store -> (Values (Listof Loc) Store)
   ))

;; ----------------------------------------
;; Syntax-object operations:
(define-signature syntax^
  (empty-ctx

   zip   ; ProperStl ProperStl Ctx -> ProperStl
   unzip ; ProperStl -> (Values ProperStl ProperStl)

   in-hole     ; Stx Stx -> Stx
   in-hole-stl ; Stl Stx -> Stl

   alloc-scope ; Symbol -> Scp

   ;; Adds or cancels a scope
   addremove ; Scp Scps -> Scps

   ;; Recursively strips lexical context from a syntax object
   strip ; Stl -> Val

   subtract ; Scps Scps -> Scps
   union    ; Scps Scps -> Scps

   binding-lookup ; (Setof StoBind) Scps -> (Option Nam)
   biggest-subset ; Scps (Listof Scps) -> Scps

   ;; Simply pushes scopes down through a syntax object
   add      ; Stx Scp -> Stx
   add-stl  ; Stl Scp -> Stl
   ;; Pushes flipping a scope down through a syntax object
   flip     ; Stx Scp -> Stx
   flip-stl ; Stl Scp -> Stl

   at-phase   ; Ctx Ph -> Scps
   ;; Updates the mapping of a `ctx` at a particular phase
   update-ctx ; Ctx Ph Scps -> Ctx
   ;; Recursively removes a set of scopes from a syntax object at a given phase
   prune      ; Ph Stx Scps -> Stx
   ))
