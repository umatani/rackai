#lang racket
(require
 "../../../mix.rkt"
 (only-in "../../../terms.rkt"
          [terms^       common:terms^]
          [terms@       common:terms@]
          [#%term-forms common:#%term-forms]))
(provide terms^ terms@ #%term-forms)

(define-signature terms^ extends common:terms^ ())

(define-mixed-unit terms@
  (import)
  (export terms^)
  (inherit [common:terms@
            Val% Atom% List%
            Var% Fun% App% If% VFun% LBind2% Bool% Num% Sym% Prim%  Null% Pair%
            Stx% Defs% AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Stxξ% Σ% StoBind% 𝓁% Hole% κ% InEval% ζ%]))

(define-syntax #%term-forms
  (syntax-local-value #'common:#%term-forms))
