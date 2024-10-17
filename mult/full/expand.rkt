#lang racket
(require
 (only-in "../../mix.rkt"                define-mixed-unit)
 (only-in "../../misc.rkt"               union)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/expand.rkt" [==> base:==>]))
(provide ==> red@ expand/red@ expand@)

(define-reduction (==> -->) #:super (base:==> --> <-)
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

  ;; application (free var-ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name <- (resolve ph stx_fun Σ)
   #:with   at := (results (lookup-ξ ξ name))
   #:when (and (set-empty? at)
               (not (member name
                            '(lambda let quote syntax let-syntax if
                               #%app #%kont #%seq #%ls-kont #%snoc))))
   #:with             id_app := (Stx (Sym '#%app) ctx)
   #:with (values 𝓁_new Σ_1) := (push-κ Σ stx κ0)
   (ζ (Stxξ ph (Stx (Lst id-seq stx-nil stx_fun . stl_args) ctx) ξ) '∘
       (κ (Stx (Pair id_app (Hole)) ctx) '• Σ*_0 𝓁_new)
       (Σ* Σ_1 scps_p (set)))
   ex-app-free-var]

  ;; reference
  ;; set-basedにすることにより，bind-syntaxesがbinding storeに多重化をもたらし，
  ;; 名前の解決が正しくできなくなる．その場合(atがempty)が生じたら
  ;; unbound errorで停止するのではなく，探索候補から削除する．
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam <- (resolve ph id Σ)
   #:with  at <- (lookup-ξ ξ nam)
   #:when (TVar? at)
   (ζ (TVar-id at) '• κ Σ*_0)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-mixed-unit expand/red@
  (import (only eval^
                -->)
          (only red^
                reducer))
  (export expand^)
  (inherit)

  (define (==> delta) (λ () (reducer (--> delta))))
  
  ; expand : Ph Stx ξ Σ* → (SetM (Cons Stx Σ*))
  (define (expand delta ph stx ξ Σ*)
    (define ==>d   (==> delta))
    (define init-ζ (ζ (Stxξ ph stx ξ) '∘ '• Σ*))
    (do ζ₀ <- (lift (apply-reduction-relation* (==>d) init-ζ))
        ;; set-baseにすることで stuck が生じる．
        ;; stuckの原因は，set-box!とbind-syntaxesがstoreへのassignmentで
        ;; あることによりstore中の値の多重化が生じること．
        (ζ stx_new '• '• Σ*_new) <- (if (and (not (InEval? ζ₀))
                                             (eq? (ζ-ex? ζ₀) '•))
                                      (pure ζ₀)
                                      (lift (set)))
        (pure (cons stx_new Σ*_new)))))

(define-compound-unit/infer expand@
  (import domain^ syntax^ env^ store^ eval^ menv^ mstore^ mcont^
          bind^ id^ parse^)
  (export expand^)
  (link expand/red@ red@))

