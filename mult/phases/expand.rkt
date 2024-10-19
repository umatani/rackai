#lang racket/base
(require
 racket/unit
 (only-in racket/match                   match)
 (only-in "../../set.rkt"                set ∅ ∅? set→list)
 (only-in "../../misc.rkt"               union)
 (only-in "../../syntax.rkt"             snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"
 (only-in "../../base/phases/expand.rkt" [==> base:==>]))
(provide ==> expand/red@ expand@)

;; ==> : ζ → (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [(only domain^
                             val? stx? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only  bind^    bind resolve)
                       (only    id^    id=?)
                       (only mcont^    push-κ)
                       (only parse^    parse)]

  ;; application (free var-ref)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args)
                               ctx)) ξ scps_p) '◯ κ0 Σ)
   #:when (id? stx_fun)
   #:with name <- (resolve ph stx_fun Σ)
   #:with   at := (results (lookup-ξ ξ name))
   #:when (and (∅? at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p) '◯
       (κ (Stx (Pair id_app (Hole)) ctx) '● 𝓁_new) Σ_1)
   ex-app-free]

  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '◯ κ0 Σ)
   #:with    nam <- (resolve ph id Σ)
   #:with     at := (results (lookup-ξ ξ nam))
   #:with id_new <- (if (∅? at)
                        (error '==>p "unbound identifier: ~a" nam)
                        (do v <- (lift at)
                            (match v
                              [(TVar id_new) (pure id_new)]
                              [_ (error '==>p "unbound identifier: ~a" nam)])))
   (ζ id_new '● κ0 Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-unit expand/red@
  (import (only eval^    -->)
          (only  red^    reducer))
  (export expand^)
  
  ;; ==> : δ → ζ → (Setof ζ)
  (define (==> δ) (reducer (--> δ)))

  ; expand : Ph Stx ξ Scps Σ → (SetM (Cons Stx Σ))
  (define (expand δ ph stx ξ scpsₚ Σ)
    (define ==>δ (==> δ))
    (define ζᵢ   (ζ (Stxξ ph stx ξ scpsₚ) '◯ '● Σ))
    
    (do (ζ stx′ '● '● Σ′) <- (lift (apply-reduction* ==>δ ζᵢ))
        (pure (cons stx′ Σ′)))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^ menv^ mstore^
          mcont^ bind^ id^ parse^)
  (export expand^)
  (link   expand/red@ red@))
