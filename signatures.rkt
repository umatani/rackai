#lang racket/base
(require
 (only-in racket/unit define-signature))
(provide (all-defined-out))


;; Add a binding using the name and scopes of an identifier, mapping
;; them to a given name in the binding store
(define-signature bind^
  (bind          ;    Σ Id Nam   → Σ                              (core)
                 ; Ph Σ Id Nam   → Σ                              (phases, full)
   resolve       ;    Σ Id       → Nam                            (core)
                 ; Ph Σ Id       → Nam                            (phases, full)
   ))

;; Common operations sensitive to contexts
(define-signature common^
  (push-cont     ; Store Cont → (Values Loc Store)
   push-κ        ; Σ κ → (Values 𝓁 Σ)
   regist-vars   ;    Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)  (core)
                 ; Ph Scp ProperStl ξ Σ → (Values ProperStl ξ Σ)  (phases, full)
   ))

;; for debug
(define-signature debug^
  (expand==>     ; δ Sexp → (Setof ζ)
   expand==>*    ; δ Sexp → (Setof ζ)
   eval-->       ; δ Sexp → (Setof State)
   eval-->*      ; δ Sexp → (Setof State)
   ))

;; ----------------------------------------
;; Implementation of Domains:
(define-signature domain^
  (δ             ; Prim (Listof Val) → Val
   α             ; (Setof Val) → (Setof Val)
   ≤ₐ            ; (Setof Val) (Setof Val) → Boolean
   val?          ; Ast → Boolean
   stx?          ; Val → Boolean
   stl?          ; Val → Boolean
   proper-stl?   ; Val → Boolean
   ))

;; ----------------------------------------
;; Environment:
(define-signature env^
  (init-env      ; → Env
   lookup-env    ; Env Var → Loc
   extend-env*   ; Env (Listof Var) (Listof Loc) → Env
   ))

;; ----------------------------------------
;; Evaluating AST:
(define-signature eval^
  (-->           ; δ →   State → (Setof State)                  (core, phases)
                 ; δ → → State → (Setof State)                  (full)
   ))

;; ----------------------------------------
;; The evaluator:
(define-signature evaluator^
  (evaluator     ; δ Ast → (SetM Val)
   ))

;; ----------------------------------------
;; The expand:
(define-signature expand^
  (==>           ; δ →   ζ → (Setof ζ)                          (core, phases)
                 ; δ → → ζ → (Setof ζ)                          (full)
   ))

;; ----------------------------------------
;; The expander:
(define-signature expander^
  (expander      ; δ Stx → (SetM (Cons Stx Σ))
   ))

;;;; reader & printer
(define-signature io^
  (reader        ; Sexp → Stx
   printer       ; Val → Sexp
   ))

;; ----------------------------------------
;; Expand-time environment operations:
(define-signature menv^
  (init-ξ        ; → ξ
   lookup-ξ      ; ξ Nam              → AllTransform
   extend-ξ      ; ξ Nam AllTransform → ξ
   ))

(define-signature misc^
  (
   lookup-cont   ; Store Loc → Cont
   lookup-val    ; Store Loc → Val
   lookup-κ      ; Σ 𝓁 → κ
   ))


(define-signature mstore^
  (;; ----------------------------------------
   ;; Expand-time store operations:
   init-Σ        ; → Σ
   lookup-Σ      ; Σ Nam → (Setof StoBind)
                 ; Σ 𝓁   → (U Val ξ κ)
   update-Σ      ; Σ Nam (Setof StoBind) → Σ
                 ; Σ 𝓁   (U Val ξ κ)     → Σ


   ;; ----------------------------------------
   ;; Alloc name, scope, and 𝓁 for expander:
   alloc-name    ; Id     Σ → (Values Nam Σ)
   alloc-scope   ; Symbol Σ → (Values Scp Σ)
   alloc-𝓁       ; Stx    Σ → (Values 𝓁   Σ)
   ))

;; ----------------------------------------
;; Simple parsing of already-expanded code
(define-signature parse^
  (parse1        ;    Stx Σ →         Ast               (core)
                 ; Ph Stx Σ →         Ast               (phases, full)
   parse*        ;    Stl Σ → (Listof Ast)              (core)
                 ; Ph Stl Σ → (Listof Ast)              (phases, full)
   parse         ;    Stx Σ → (SetM   Ast)              (core)
                 ; Ph Stx Σ → (SetM   Ast)              (phases, full)
   ))

(define-signature parser^
  (parser        ; Stx Σ → (SetM Ast)
   ))

;;;; runner
(define-signature run^
  (run           ; δ Sexp Symbol → (Setof Val)
   ))

;; ----------------------------------------
;; Store:
(define-signature store^
  (init-store    ; → Store
   lookup-store  ; Store Loc              → (U Val Cont)
   update-store  ; Store Loc (U Val Cont) → Store
   alloc-loc     ; Symbol       Store → (Values         Loc  Store)
   ))

;; ----------------------------------------
;; Syntax-object operations:
(define-signature syntax^
  (empty-ctx     ; → Ctx
   zip           ; ProperStl ProperStl Ctx → ProperStl
   unzip         ; ProperStl → (Values ProperStl ProperStl)
   in-hole       ; Stx Stx → Stx
   strip         ; Stl → Val
   add           ; Stx Scp -> Stx
   flip          ; Stx Scp -> Stx
   at-phase      ; Ctx Ph -> Scps                       (phases, full)
   update-ctx    ; Ctx Ph Scps -> Ctx                   (phases, full)
   prune         ; Ph Stx Scps -> Stx                   (phases, full)
   ))
