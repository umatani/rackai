#lang racket
(require
 "../../../mix.rkt"
 (only-in "../../../term.rkt" define-term)
 (only-in "../core/terms.rkt"
          [terms^       core:terms^]
          [terms@       core:terms@]
          [#%term-forms core:#%term-forms]))
(provide terms^ terms@ #%term-forms)

(define-signature terms^ extends core:terms^ ())

(define-mixed-unit terms@
  (import)
  (export terms^)
  (inherit [core:terms@
            [core:Stxξ% Stxξ%]
            Val% Atom% List%
            Var% Fun% App% If% VFun% LBind2% Bool% Num% Sym% Prim% Null% Pair%
            Stx% Defs% AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% 𝓁% Hole% κ% InEval% ζ%])
  ;;; updated (ph scps
  (define-term Stxξ core:Stxξ (ph scps)))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'core:#%term-forms)))
