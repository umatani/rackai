#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match)
 "../../interpreter.rkt"
 "../../test/suites.rkt"
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../misc.rkt"             union)
 (only-in "../../set.rkt"              set ∅ ∅? set→list)
 (only-in "../../syntax.rkt"           snoc)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../base/full/terms.rkt"

 (only-in "../../mult/full/units.rkt"  eval/red@ expand/red@
                                       [parse@ mult:parse@] parser@)
 (only-in "../../mult/full/eval.rkt"   [--> mult:-->])
 (only-in "../../mult/full/expand.rkt" [==> mult:==>])
 (only-in "../full.rkt"                main-minus@)
 (only-in "domain.rkt"                 domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                       stx-⊤ list-⊤))
(provide interp)


;;;; Eval

(define-reduction (--> δ ==>) #:super (mult:--> δ ==>)
  #:within-signatures [(only syntax^
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
  ;; (syntax-local-value <abs> _ ...)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-value _) ,id ,_ ...) '())
     ,cont ,store ,(and Σ̂_0 (Σ̂ Σ _ _)))
   #:when (or (equal? id val-⊤) (equal? id atom-⊤)
              (equal? id stx-⊤))
   `(,val-⊤ ,cont ,store ,Σ̂_0)
   ev-lval-abs]

  ;; (syntax-local-identifier-as-binding <abs>)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-identifier-as-binding _) ,id) '())
     ,cont ,store ,(and Σ̂_0 (Σ̂ _ _ scps_u)))
   #:when (or (equal? id val-⊤)
              (equal? id atom-⊤)
              (equal? id stx-⊤)
              (and (Stx? id) (equal? (Stx-e id) sym-⊤)))
   `(,stx-⊤ ,cont ,store ,Σ̂_0)
   ev-lbinder-abs]

  ;; (syntax-local-bind-syntaxes <abs> <abs> <abs>)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-bind-syntaxes _)
              ,ids ,rhs ,defs) '()) ,cont ,store ,Σ̂_0)
   #:when (or (or (equal? ids list-⊤)
                  (and (Pair? ids) (Null? (Pair-d ids))
                       (let ([id (Pair-a ids)])
                         (or (equal? id val-⊤)
                             (equal? id atom-⊤)
                             (equal? id stx-⊤)
                             (and (Stx? id) (equal? (Stx-e id) sym-⊤))))))
              (or (equal? rhs (Bool #f))
                  (equal? rhs val-⊤)
                  (equal? rhs atom-⊤)
                  (equal? rhs stx-⊤))
              (or (equal? defs val-⊤)
                  (equal? defs atom-⊤)))
   `(,list-⊤ ,cont ,store ,Σ̂_0)
   ev-slbs-abs]
  
  ;; (local-expand <abs> contextv idstops defs?)
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'local-expand _)
              ,stx ,val_contextv ,val_idstops ,val_defs ...) '())
     ,cont ,store ,(and Σ̂_0 (Σ̂ Σ _ _)))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   `(,stx-⊤ ,cont ,store ,Σ̂_0)
   ev-lexpand-abs]

  ;; β (<abs> ...)
  [`(,(SApp _lbl `(,_ph ,_maybe-scp_i ,_ξ) (cons f _) '()) ,cont ,store ,Σ̂)
   #:when (equal? f val-⊤)
   `(,f ,cont ,store ,Σ̂)
   ev-β-abs]

  ;; (if <abs> ...)
  [`(,(SIf _lbl v _ tm_else) ,cont ,store ,Σ̂)
   #:when (or (equal? v val-⊤)
              (equal? v atom-⊤))
   `(,tm_else ,cont ,store ,Σ̂)
   ev-if-abs-#f])

(define-unit-from-reduction ev:red@ -->)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Ph Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) ph stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
      (pure val-⊤)
      ((mult:parse1 prs1 prs*) ph stx Σ)))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Expander

(define-reduction (==> -->) #:super (mult:==> -->)
  #:within-signatures [(only syntax^
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
  
  [(InEval (list stx_exp '● store_0 Σ̂)
           (ζ (Stxξ ph (Stx (Bool #f) ctx_i) ξ) '◯ κ _))
   #:when (or (equal? stx_exp val-⊤)
              (equal? stx_exp atom-⊤)
              (equal? stx_exp stx-⊤))
   (ζ (Stxξ ph stx_exp ξ) '◯ κ Σ̂)
   ex-macapp-abs]

  ;; abstract value
  [(ζ (Stxξ ph val ξ) '◯ κ Σ̂)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   (ζ val '● κ Σ̂)
   ex-abs-⊤])

(define-unit-from-reduction ex:red@ ==>)

;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         (() eval/red@   ev) (([ev : red^]) ev:red@)
         parse@ parser@
         (() expand/red@ ex) (([ex : red^]) ex:red@)))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'full   interp)
  (run-suite 'finite interp))
