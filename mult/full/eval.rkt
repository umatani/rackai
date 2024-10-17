#lang racket
(require
 (only-in "../../set.rkt"            set)
 (only-in "../../misc.rkt"           union)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../base/full/eval.rkt" [--> base:-->]))
(provide --> red@ eval/red@ eval@)

;; --> : State -> (Setof State)
(define-reduction (--> delta ==>) #:super (base:--> delta ==> <-)
  #:within-signatures [(only domain^
                             val? stx?)
                       (only syntax^
                             add flip prune)
                       (only env^
                             init-env lookup-env extend-env*)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-scope alloc-𝓁 lookup-Σ update-Σ)
                       (only  bind^    bind resolve)
                       (only parse^    parse)]
  #:do [; resolve* : Ph (Listof Id) Σ -> (SetM (Listof Nam))
        (define (resolve* ph ids Σ)
          (match ids
            ['() (pure '())]
            [(cons id ids*)
             (do nam  <- (resolve  ph id Σ)
                 nams <- (resolve* ph ids* Σ)
                 (pure (cons nam nams)))]))

        ;; lookup-ξ* : ξ (Listof Nam) -> (SetM (Listof AllTransform))
        (define (lookup-ξ* ξ ns)
          (match ns
            ['() (pure '())]
            [(cons n ns*)
             (do  a <- (let ([as (lookup-ξ  ξ n)])
                         (if (set-empty? (results as))
                             (pure 'not-found)
                             as))
                 as <- (lookup-ξ* ξ ns*)
                 (pure (cons a as)))]))])

(define-unit-from-reduction red@ -->)

(define-unit eval/red@
  (import (only domain^
                val?)
          (only env^
                init-env)
          (only store^
                init-store)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                ==>)
          (only red^
                reducer))
  (export eval^)

  ;; --> : δ → → State → (Setof State)
  (define (--> delta) (λ () (reducer delta (==> delta))))

  ;; eval : Ph Ast MaybeScp ξ Σ* → (SetM (Cons Val Σ*))
  (define (eval delta ph ast maybe-scp_i ξ Σ*)
    (define -->d (--> delta))
    (do `(,(? val? val) • ,_store ,Σ*_2) <- (lift
                                             (apply-reduction-relation*
                                              (-->d)
                                              `(,(AstEnv ph ast (init-env)
                                                         maybe-scp_i ξ)
                                                • ,(init-store) ,Σ*)))
        (pure (cons val Σ*_2))))

  ;; evaluate : Ast → (SetM Val)
  (define (evaluate delta ast)
    (do (cons val _Σ*) <- (eval delta 0 ast 'no-scope (init-ξ)
                                (Σ* (init-Σ) (set) (set)))
        (pure val))))

(define-compound-unit/infer eval@
  (import domain^ syntax^ env^ store^ cont^ menv^ mstore^ bind^ expand^ parse^)
  (export eval^)
  (link eval/red@ red@))
