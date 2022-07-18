#lang racket
(require
 (only-in "term.rkt" define-term use-terms))
(provide config^ config@ #%term-forms)

;; ----------------------------------------
;; Internal Configuration

(define-signature config^
  (;; Eval-time continuation, environment, and store
   AstEnv% Store% KApp% KIf% SApp% SIf%

   ;; SSeq is used only in full
   SSeq%

   ;; Expand-time environment
   TVar% TStop%

   ;; Expand-time store
   Σ% StoBind%

   ;; Expand-time continuation
   Stxξ% κ%

   ;; Expand-time state (configuration)
   InEval% ζ%))

(define-unit config@
  (import) (export config^)

  ;; Eval-time continuation, environment, and store
  (define-term AstEnv  (ast env))
  (define-term Store   (size tbl))
  (define-term KApp    (lbl vals tms loc))
  (define-term KIf     (lbl thn els loc))
  (define-term SApp    (lbl vals tms))
  (define-term SIf     (lbl tst thn els))
  ;; SSeq is used only in full
  (define-term SSeq    (tms))

  ;; Expand-time environment
  (define-term TVar    (id))
  (define-term TStop   (all-transform))

  ;; Expand-time store
  (define-term Σ       (size tbl))
  (define-term StoBind (scps nam))

  ;; Expand-time continuation
  (define-term Stxξ    (stx ξ))
  (define-term κ       (stx ex? 𝓁))

  ;; Expand-time state (configuration)
  (define-term InEval  (state ξ))
  (define-term ζ       (stx ex? κ Σ)))

(define-syntax #%term-forms
  '((AstEnv  ast env)
    (Store   size tbl)
    (KApp    lbl vals tms loc)
    (KIf     lbl thn els loc)
    (SApp    lbl vals tms)
    (SIf     lbl tst thn els)
    (SSeq    tms)
    (TVar    id)
    (TStop   all-transform)
    (Σ       size tbl)
    (StoBind scps nam)
    (Stxξ    stx ξ)
    (κ       stx ex? 𝓁)
    (InEval  state ξ)
    (ζ       stx ex? κ Σ)))
