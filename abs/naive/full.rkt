#lang racket
(require
 "../../reduction.rkt"
 "../../test/suites.rkt"

 (only-in "../../signatures.rkt"
          syntax^ env^ store^ cont^ menv^ mstore^ bind^ mcont^ parser^
          domain^ run^ debug^)
 (only-in "../../interp-base/full/terms.rkt"
          Var% Fun% App% If% VFun% Atom% Bool% Sym% Stx% List%
          Null% Pair% Prim%
          KApp% KIf% SApp% SIf% AstEnv% κ% Stxξ% ζ% 𝓁% Σ% Σ*% Hole%
          TVar% TStop% Defs% InEval% InExpand%
          Lst lst->list snoc id? prim? stx-prim?)
 
 (only-in "../../interp-set/full/units.rkt" eval/red@ expand/red@)
 (only-in "../../interp-set/full/eval.rkt" [--> set:-->])
 (only-in "../../interp-set/full/expander.rkt" [==> set:==>])
 (only-in "../full.rkt" main-minus@)
 (only-in "domain.rkt" domain@ val-⊤ atom-⊤ stx-⊤ val? proper-stl?))
(provide run delta α ≤a)


(define-reduction (--> delta ==>) #:super (set:--> delta ==>)
  #:within-signatures [(only syntax^
                             add flip union alloc-scope prune)
                       (only env^
                             init-env lookup-env extend-env)
                       (only store^
                             lookup-store update-store* alloc-loc*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-𝓁 lookup-Σ update-Σ)
                       (only bind^
                             bind resolve)
                       (only parser^
                             parse)]
  ;; (syntax-local-value <abs> _ ...)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-value _) ,id ,_ ...) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:when (or (equal? id val-⊤) (equal? id atom-⊤)
              (equal? id stx-⊤))
   `(,val-⊤ ,cont ,store ,Σ*_0)
   ev-lval-abs]

  ;; (local-expand <abs> contextv idstops defs?)
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'local-expand _)
              ,stx ,val_contextv ,val_idstops ,val_defs ...) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   `(,stx-⊤ ,cont ,store ,Σ*_0)
   ev-lexpand-abs]


  ;; β (<abs> ...)
  [`(,(SApp _lbl `(,_ph ,_maybe-scp_i ,_ξ) (cons f _) '()) ,cont ,store ,Σ*)
   #:when (equal? f val-⊤)
   `(,f ,cont ,store ,Σ*)
   ev-β-abs]

  ;; (if <abs> ...)
  [`(,(SIf _lbl (? (λ (v) (or (equal? v val-⊤) (equal? v atom-⊤))))
           _ tm_else) ,cont ,store ,Σ*)
   `(,tm_else ,cont ,store ,Σ*)
   ev-if-abs-#f]
  )

(define-unit-from-reduction ev:red@ -->)


(define-reduction (==> -->) #:super (set:==> -->)
  #:within-signatures [(only syntax^
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
  
  [(InEval (list (? Stx? stx_exp) '• store_0 Σ*)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ) '∘ κ _))
   (if (equal? stx_exp stx-⊤)
       (ζ (Stxξ ph stx_exp ξ) '∘ κ Σ*)
       (let ([scp_i (car (set->list (at-phase ctx_i ph)))])
         (ζ (Stxξ ph (flip ph stx_exp scp_i) ξ) '∘ κ Σ*)))
   ex-macapp-flip]

  ;; stx-⊤
  [(ζ (Stxξ ph (and stx (Stx 'stx-⊤ _)) ξ) '∘ κ Σ*)
   (ζ stx '• κ Σ*)
   ex-stx-⊤])

(define-unit-from-reduction ex:red@ ==>)

;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link main-minus@
         (() eval/red@   ev) (([ev : red^]) ev:red@)
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))
