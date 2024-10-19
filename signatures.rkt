#lang racket/base
(require
 (only-in racket/unit define-signature))
(provide (all-defined-out))


;; Add a binding using the name and scopes of an identifier, mapping
;; them to a given name in the binding store
(define-signature bind^
  (bind          ;    Σ Id Nam   → Σ                            (core)
                 ; Ph Σ Id Nam   → Σ                            (phases, full)
   resolve       ;    Σ Id       → Nam                          (core)
                 ; Ph Σ Id       → Nam                          (phases, full)
   ))

;; ----------------------------------------
 ;; Continuation:
(define-signature cont^
  (push-cont     ; Store Cont → (Values Loc Store)
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
   α             ; Val → (Setof Val)
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
   evaluate      ; δ Ast → Val                                  (base)
                 ; δ Ast → (SetM Val)                           (mult)
   ))

;; ----------------------------------------
;; The expand:
(define-signature expand^
  (==>           ; δ →   ζ → (Setof ζ)                          (core, phases)
                 ; δ → → ζ → (Setof ζ)                          (full)
   expand        ; δ    Stx ξ      Σ  →       (Cons Stx Σ )     (base/core)
                 ; δ Ph Stx ξ Scps Σ  →       (Cons Stx Σ )     (base/phases)
                 ; δ Ph Stx ξ      Σ* →       (Cons Stx Σ*)     (base/full)
                 ; δ    Stx ξ      Σ  → (SetM (Cons Stx Σ ))    (mult/core)
                 ; δ Ph Stx ξ Scps Σ  → (SetM (Cons Stx Σ ))    (mult/phases)
                 ; δ Ph Stx ξ      Σ* → (SetM (Cons Stx Σ*))    (mult/full)
   ))

;; ----------------------------------------
;; The expander:
(define-signature expander^
  (expander      ; δ Stx →       (Cons Stx Σ)                   (base)
                 ; δ Stx → (SetM (Cons Stx Σ))                  (mult)
   ))

(define-signature id^
  (id=?          ;    Id Nam   Σ → Boolean                      (core)
                 ; Ph Id Nam   Σ → Boolean                      (phases)
                 ; Ph Id Nam ξ Σ → Boolean                      (full)
   core-form?    ;       Nam   Σ → Id → Boolean                 (core)
                 ; Ph    Nam   Σ → Id → Boolean                 (phases, full)
   ))

;;;; reader & printer
(define-signature io^
  (reader        ; Sexp → Stx
   printer       ; Val → Sexp
   ))

;; ----------------------------------------
;; Expand-time call stack operations:
(define-signature mcont^
  (push-κ        ; Σ κ → (Values 𝓁 Σ)
   ))

;; ----------------------------------------
;; Expand-time environment operations:
(define-signature menv^
  (init-ξ        ; → ξ
   lookup-ξ      ; ξ Nam              → AllTransform
   extend-ξ      ; ξ Nam AllTransform → ξ
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
   parse         ;    Stx Σ →       Ast                 (base/core)
                 ; Ph Stx Σ →       Ast                 (base/phases, base/full)
                 ;    Stx Σ → (SetM Ast)                (mult/core)
                 ; Ph Stx Σ → (SetM Ast)                (mult/phases, mult/full)
   ))

(define-signature parser^
  (parser        ; Stx Σ →       Ast                    (base)
                 ; Stx Σ → (SetM Ast)                   (mult)
   ))

;;;; runner
(define-signature run^
  (run           ; δ Sexp Symbol →        Val           (base)
                 ; δ Sexp Symbol → (Setof Val)          (mult)
   ))

;; ----------------------------------------
;; Store:
(define-signature store^
  (init-store    ; → Store
   lookup-store  ; Store Loc              → (U Val Cont)
   update-store  ; Store Loc (U Val Cont) → Store
   update-store* ; Store (Listof Loc) (Listof (U Val Cont)) → Store

   alloc-loc     ; Symbol       Store → (Values         Loc  Store)
   alloc-loc*    ; (Listof Nam) Store → (Values (Listof Loc) Store)
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
