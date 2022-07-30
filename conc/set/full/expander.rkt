#lang racket
(require
 "../../../set.rkt"
 "../../../reduction.rkt"
 "../../../mix.rkt"
 (only-in "../../../term.rkt"  use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ env^ store^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^ expander^)
 (only-in "../../../terms.rkt"
          App% Atom% Sym% Stx% List% Null% Pair% Hole%
          lst->list snoc id? prim?
          use-lst-form)
 (only-in "../../base/full/config.rkt" config^ #%term-forms)
 (only-in "../../base/full/expander.rkt" [==> base:==>] expander/expand@))
(provide ==> expander@)

(define-reduction (==> -->) #:super (base:==> --> <-)
  #:within-signatures [(only config^
                             AstEnv% TVar% TStop% ζ% Stxξ% κ% Σ*% InEval%)
                       (only terms-extra^
                             val? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip add flip union in-hole
                             alloc-scope prune at-phase)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parser^
                             parse)]

  ;; application (free var-ref, same as phases)
  [(ζ (Stxξ ph (and stx (Stx (Lst stx_fun . stl_args) ctx)) ξ) '∘
       κ0 (and Σ*_0 (Σ* Σ scps_p _)))
   #:when (id? stx_fun)
   #:with name <- (resolve #:phase ph stx_fun Σ)
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

  ;; reference (same as phases)
  [(ζ (Stxξ ph (and id (Stx (Sym nam) ctx)) ξ) '∘
       κ (and Σ*_0 (Σ* Σ _ _)))
   #:with nam <- (resolve #:phase ph id Σ)
   #:with  ats := (results (lookup-ξ ξ nam))
   ;(printf "ats: ~a\n" ats)
   #:with at <- (lift ats)
   #:when (TVar? at)
   (ζ (TVar-id at) '• κ Σ*_0)
   ex-var])

(define-unit-from-reduction red@ ==>)

(define-mixed-unit expander@
  (import (only config^
                Stxξ% Σ*% ζ% InEval%)
          (only eval^
                -->))
  (export expand^ expander^)
  (inherit [red@ reducer]
           [expander/expand@ expander])
  (use-terms Stxξ Σ* ζ InEval)

  (define (==> delta) (λ () (reducer (--> delta))))
  
  ; expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*))
  (define (expand delta ph stx ξ Σ*)
    (define ==>d (==> delta))
    (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• Σ*)])
      (define ζs (apply-reduction-relation* (==>d) init-ζ))
      (define succs (filter (λ (ζ0)
                              (and (not (InEval? ζ0))
                                   (eq? (ζ-ex? ζ0) '•)))
                            (set->list ζs)))
      (printf "expand: ~a ~a\n" (set-count ζs) (length succs))
      (match-let ([(list (ζ stx_new '• '• Σ*_new) ...) succs])
        (list->set (map cons stx_new Σ*_new))))))
