#lang racket
(require
 "../../../mix.rkt"
 (only-in "../../../term.rkt" define-term)
 (only-in "../phases/terms.rkt"
          [terms^       phases:terms^]
          [terms@       phases:terms@]
          [#%term-forms phases:#%term-forms]))
(provide terms^ terms@ #%term-forms)

(define-signature terms^ extends phases:terms^
  (Σ*% InExpand%))

(define-mixed-unit terms@
  (import)
  (export terms^)
  (inherit [phases:terms@
            [phases:AstEnv% AstEnv%]
            [phases:Stxξ%   Stxξ%]
            [phases:KApp%   KApp%]
            [phases:SApp%   SApp%]
            [phases:κ%      κ%]
            [phases:ζ%      ζ%]
            Val% Atom%
            Var% Fun% App% If% VFun% LBind2% Bool% Num% Sym% Defs%
            Stx%  Store% KIf% SIf% SSeq%
            TVar% TStop% Σ% StoBind% 𝓁% Hole% InEval%])
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
  (define-term ζ        phases:ζ      (Σ*) #:remove [Σ]))


(define-syntax #%term-forms
  (append '((AstEnv   ph ast env maybe-scp ξ)
            (Stxξ     ph stx ξ)
            (Σ*       Σ scps_p scps_u)
            (KApp     ctx lbl vals tms loc)
            (SApp     ctx lbl vals tms)
            (InExpand ζ state)
            (κ        stx ex? Σ* 𝓁)
            (ζ        stx ex? κ Σ*))
          (syntax-local-value #'phases:#%term-forms)))
