#lang racket
(require
 (only-in "../../../term.rkt" define-term)
 (only-in "../core/terms.rkt"
          [terms^ core:terms^]
          [terms@ core:terms@]
          [#%term-forms core:#%term-forms]))
(provide (all-defined-out))

(define-signature terms^ extends core:terms^
  ())

(define-unit terms-phases@
  (import (prefix core: core:terms^))
  (export terms^)
  
  ;;; updated (ph scps
  (define-term Stxξ    core:Stxξ    (ph scps))

  ;; same as common
  (define-term Var     core:Var     ())
  (define-term Fun     core:Fun     ())
  (define-term App     core:App     ())
  (define-term If      core:If      ())
  (define-term VFun    core:VFun    ())
  (define-term LBind2  core:LBind2  ())
  (define-term Sym     core:Sym     ())
  (define-term Defs    core:Defs    ())
  (define-term Stx     core:Stx     ())
  (define-term AstEnv  core:AstEnv  ())
  (define-term Store   core:Store   ())
  (define-term KApp    core:KApp    ())
  (define-term KIf     core:KIf     ())
  (define-term SApp    core:SApp    ())
  (define-term SIf     core:SIf     ())
  (define-term SSeq    core:SSeq    ())
  (define-term TVar    core:TVar    ())
  (define-term TStop   core:TStop   ())
  (define-term Σ       core:Σ       ())
  (define-term StoBind core:StoBind ())
  (define-term 𝓁       core:𝓁       ())
  (define-term Hole    core:Hole    ())
  (define-term κ       core:κ       ())
  (define-term InEval  core:InEval  ())
  (define-term ζ       core:ζ       ()))

(define-compound-unit/infer terms@
  (import) (export terms^)
  (link (([ct : core:terms^]) core:terms@)
        (() terms-phases@ ct)))

(define-syntax #%term-forms
  (append '((Stxξ ph stx ξ scps))
          (syntax-local-value #'core:#%term-forms)))
