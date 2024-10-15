#lang racket
(require
 (only-in "../../set.rkt"                  set)
 (only-in "../../mix.rkt"                  define-mixed-unit)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../conc/phases/terms.rkt"
 (only-in "../../conc/phases/expander.rkt" [==> base:==>] expander@))
(provide ==> expand/red@ expand@)

;; ==> : ζ → (Setof ζ)
(define-reduction (==> -->) #:super (base:==> <- -->)
  #:within-signatures [(only domain^
                             val? stx? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name alloc-scope)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parse^    parse)]

  ;; application (free var-ref)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args)
                               ctx)) ξ scps_p) '∘ κ0 Σ)
   #:when (id? stx_fun)
   #:with name <- (resolve ph stx_fun Σ)
   #:with   at := (results (lookup-ξ ξ name))
   #:when (and (set-empty? at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%ls-kont #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args)
                      ctx) ξ scps_p) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• 𝓁_new) Σ_1)
   ex-app-free-var]

  ;; reference
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ scps_p) '∘ κ0 Σ)
   #:with    nam <- (resolve ph id Σ)
   #:with     at := (results (lookup-ξ ξ nam))
   #:with id_new <- (if (set-empty? at)
                        (error '==>p "unbound identifier: ~a" nam)
                        (do v <- (lift at)
                            (match v
                              [(TVar id_new) (pure id_new)]
                              [_ (error '==>p "unbound identifier: ~a" nam)])))
   (ζ id_new '• κ0 Σ)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand/red@
  (import (only eval^
                -->)
          (only red^
                reducer))
  (export expand^)
  (inherit)
  
  (define (==> delta) (reducer (--> delta)))

  ; expand : Ph Stx ξ Scps Σ -> (Setof (Cons Stx Σ))
  (define (expand delta ph stx ξ scps_p Σ)
    (define ==>d (==> delta))
    (let ([init-ζ (ζ (Stxξ ph stx ξ scps_p) '∘ '• Σ)])
      (match-let ([(set (ζ stx_new '• '• Σ_new) ...)
                   (apply-reduction-relation* ==>d init-ζ)])
        (list->set (map cons stx_new Σ_new))))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^ menv^ mstore^
          mcont^ bind^ parse^)
  (export expand^)
  (link expand/red@ red@))
