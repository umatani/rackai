#lang racket
(require
 (only-in "../../../term.rkt" define-term)
 (only-in "../../../terms.rkt"
          [terms^ common:terms^]
          [terms@ common:terms@]
          [#%term-forms common:#%term-forms]))
(provide (all-defined-out))

(define-signature terms^ extends common:terms^
  ())

(define-unit terms-core@
  (import (prefix common: common:terms^))
  (export terms^)
  
  ;; same as common
  (define-term Var     common:Var     ())
  (define-term Fun     common:Fun     ())
  (define-term App     common:App     ())
  (define-term If      common:If      ())
  (define-term VFun    common:VFun    ())
  (define-term LBind2  common:LBind2  ())
  (define-term Sym     common:Sym     ())
  (define-term Defs    common:Defs    ())
  (define-term Stx     common:Stx     ())
  (define-term AstEnv  common:AstEnv  ())
  (define-term Store   common:Store   ())
  (define-term KApp    common:KApp    ())
  (define-term KIf     common:KIf     ())
  (define-term SApp    common:SApp    ())
  (define-term SIf     common:SIf     ())
  (define-term SSeq    common:SSeq    ())
  (define-term TVar    common:TVar    ())
  (define-term TStop   common:TStop   ())
  (define-term Stxξ    common:Stxξ    ())
  (define-term Σ       common:Σ       ())
  (define-term StoBind common:StoBind ())
  (define-term 𝓁       common:𝓁       ())
  (define-term Hole    common:Hole    ())
  (define-term κ       common:κ       ())
  (define-term InEval  common:InEval  ())
  (define-term ζ       common:ζ       ()))

(define-compound-unit/infer terms@
  (import)
  (export terms^)
  (link (([ct : common:terms^]) common:terms@)
        (()                     terms-core@ ct)))

(define-syntax #%term-forms
  (syntax-local-value #'common:#%term-forms))
