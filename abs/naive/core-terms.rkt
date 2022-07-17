#lang racket
(require
 "../../mix.rkt"
 (only-in "../../conc/base/core/terms.rkt"
          terms^ #%term-forms
          [terms@ super:terms@])
 (only-in "../../terms.rkt"
          terms-extra^
          [terms-extra@ super:terms-extra@]))
(provide terms^ terms@ #%term-forms terms-extra@)

(define-mixed-unit terms@
  (import)
  (export terms^)
  (inherit [super:terms@
            Var% Fun% App% If% Val% Atom% VFun%
            LBind2% Bool% Num% Sym% Defs% Stx%
            AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% 𝓁% Stxξ% Hole% κ%
            InEval% ζ%]))

(define-mixed-unit terms-extra@
  (import)
  (export terms-extra^)
  (inherit [super:terms-extra@
            val? prim? stx-prim? atom? id id? stx? stl? proper-stl?
            nam? state? tm? cont? ser?]))
