#lang racket
(require
 (only-in "../../../term.rkt" define-term)
 (only-in "../phases/terms.rkt"
          [terms^ phases:terms^]
          [terms@ phases:terms@]
          [#%term-forms phases:#%term-forms]))
(provide terms^ terms@ #%term-forms)

(define-signature terms^ extends phases:terms^
  (Σ*% InExpand%))

(define-compound-unit terms@
  (import) (export t)
  (link
   (([ct : phases:terms^]) phases:terms@)
   (([t  : terms^])
    (unit
      (import (prefix phases: phases:terms^))
      (export terms^)
      
      ;; add ph, maybe-scp, and ξ
      (define-term AstEnv   phases:AstEnv (ph maybe-scp ξ))
      ;; remove scps from those of phases
      (define-term Stxξ     phases:Stxξ   () #:remove [scps])
      ;; new
      (define-term Σ*                     (Σ scps_p scps_u))
      ;; add ctx (List Ph MaybeScp ξ)
      (define-term KApp     phases:KApp   (ctx))
      ;; add ctx (List Ph MaybeScp ξ)
      (define-term SApp     phases:SApp   (ctx))
      ;; new
      (define-term InExpand               (ζ state))
      ;; add Σ*
      (define-term κ        phases:κ      (Σ*))
      ;; Σ -> Σ*
      (define-term ζ        phases:ζ      (Σ*) #:remove [Σ])

      ;; same as common
      (define-term Var     phases:Var     ())
      (define-term Fun     phases:Fun     ())
      (define-term App     phases:App     ())
      (define-term If      phases:If      ())
      (define-term VFun    phases:VFun    ())
      (define-term LBind2  phases:LBind2  ())
      (define-term Sym     phases:Sym     ())
      (define-term Defs    phases:Defs    ())
      (define-term Stx     phases:Stx     ())
      (define-term Store   phases:Store   ())
      (define-term KIf     phases:KIf     ())
      (define-term SIf     phases:SIf     ())
      (define-term SSeq    phases:SSeq    ())
      (define-term TVar    phases:TVar    ())
      (define-term TStop   phases:TStop   ())
      (define-term Σ       phases:Σ       ())
      (define-term StoBind phases:StoBind ())
      (define-term Θ       phases:Θ       ())
      (define-term 𝓁       phases:𝓁       ())
      (define-term Hole    phases:Hole    ())
      (define-term InEval  phases:InEval  ()))
    ct)))

(define-syntax #%term-forms
  (append '((AstEnv   ph ast env maybe-scp ξ)
            (Stxξ     ph stx ξ)
            (Σ*       Σ scps_p scps_u)
            (KApp     ctx vals tms loc)
            (SApp     ctx vals tms)
            (InExpand ζ state)
            (κ        stx ex? Σ* 𝓁)
            (ζ        stx ex? κ Θ Σ*))
          (syntax-local-value #'phases:#%term-forms)))
