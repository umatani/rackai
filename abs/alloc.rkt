#lang racket/base
(require
 racket/unit
 (only-in racket/match        match-let)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../mult/units.rkt" [ store@  mult:store@]
                              [mstore@ mult:mstore@]))
(provide mstore@ store@)


(define-mixed-unit mstore@
  (import)
  (export  mstore^)
  (inherit [mult:mstore@    init-Σ lookup-Σ update-Σ])

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  ;; alloc-name : Id Σ → (Values Nam Σ)
  (define (alloc-name id Σ)
    (match-let ([(Stx (Sym nam) _) id])
      (values nam Σ)))

  ;; alloc-scope : Nam Σ → (Values Scp Σ)
  (define (alloc-scope nam Σ)
    ;; TODO: nam (Symbol) ではなく Stx にすると精度向上
    (values nam Σ))

  ;; alloc-𝓁 : Stx Σ → (Values 𝓁 Σ)
  ;;   - called only from push-κ
  ;;   - stx is used in abs for ensuring finiteness of the domain
  (define (alloc-𝓁 stx Σ)
    (values (𝓁 stx) Σ)))

(define-mixed-unit store@
  (import)
  (export  store^)
  (inherit [mult:store@    init-store lookup-store update-store])

  ;; alloc-loc : Nam Store → (Values Loc Store)
  ;;   - called from push-cont
  ;;   - a unique lbl is generated for each App and If form during parse
  (define (alloc-loc lbl sto)
    (values lbl sto)))
