#lang racket
(require
 "../../interpreter.rkt"
 ;"../../test/suites.rkt"
 (only-in "../../mix.rkt"                define-mixed-unit)
 "../../reduction.rkt"
 "../../signatures.rkt"
 "../../conc/full/terms.rkt"
 (only-in "../../mult/full/units.rkt"    eval/red@ expand/red@
                                         [parse@ mult:parse@])
 (only-in "../../mult/full/eval.rkt"     [--> set:-->])
 (only-in "../../mult/full/expander.rkt" [==> set:==>])
 (only-in "../full.rkt"                  main-minus@)
 (only-in "domain.rkt"                   domain@ val-⊤ atom-⊤ num-⊤ sym-⊤
                                         stx-⊤ list-⊤))
(provide interp)


;;;; Eval

(define-reduction (--> delta ==>) #:super (set:--> delta ==>)
  #:within-signatures [(only syntax^
                             add flip union prune)
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
                       (only bind^
                             bind resolve)
                       (only parse^    parse)]
  ;; (syntax-local-value <abs> _ ...)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-value _) ,id ,_ ...) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:when (or (equal? id val-⊤) (equal? id atom-⊤)
              (equal? id stx-⊤))
   `(,val-⊤ ,cont ,store ,Σ*_0)
   ev-lval-abs]

  ;; (syntax-local-identifier-as-binding <abs>)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-identifier-as-binding _) ,id) '())
     ,cont ,store ,(and Σ*_0 (Σ* _ _ scps_u)))
   #:when (or (equal? id val-⊤)
              (equal? id atom-⊤)
              (equal? id stx-⊤)
              (and (Stx? id) (equal? (Stx-e id) sym-⊤)))
   `(,stx-⊤ ,cont ,store ,Σ*_0)
   ev-lbinder-abs]

  ;; (syntax-local-bind-syntaxes <abs> <abs> <abs>)
  [`(,(SApp _lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'syntax-local-bind-syntaxes _)
              ,ids ,rhs ,defs) '()) ,cont ,store ,Σ*_0)
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
   `(,list-⊤ ,cont ,store ,Σ*_0)
   ev-slbs-abs]
  
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
  [`(,(SIf _lbl v _ tm_else) ,cont ,store ,Σ*)
   #:when (or (equal? v val-⊤)
              (equal? v atom-⊤))
   `(,tm_else ,cont ,store ,Σ*)
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

(define-mixed-unit parser@
  (import)
  (export  parser^)
  (inherit [parse@ parse])

  ; parser : Stx Σ → (SetM Ast)
  (define (parser stx Σ) (parse 0 stx Σ)))


;;;; Expander

(define-reduction (==> -->) #:super (set:==> -->)
  #:within-signatures [(only syntax^
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
  
  [(InEval (list stx_exp '• store_0 Σ*)
           (ζ (Stxξ ph (Stx #f ctx_i) ξ) '∘ κ _))
   #:when (or (equal? stx_exp val-⊤)
              (equal? stx_exp atom-⊤)
              (equal? stx_exp stx-⊤))
   (ζ (Stxξ ph stx_exp ξ) '∘ κ Σ*)
   ex-macapp-flip-abs]

  ;; abstract value
  [(ζ (Stxξ ph val ξ) '∘ κ Σ*)
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤)
              (equal? val num-⊤)
              (equal? val sym-⊤)
              (equal? val stx-⊤)
              (equal? val list-⊤))
   (ζ val '• κ Σ*)
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

(define interp (interpreter 'naive:full run delta α ≤a #f))

(define (process form [mode 'eval]) ;; mode = read/expand/parse/eval
  (apply-interpreter interp form mode))


#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'finite)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))
