#lang racket
(require
 "../../mix.rkt"

 (only-in "../../signatures.rkt" delta^)
 
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
            LBind2% Bool% Num% Sym% Stx% List% Null% Pair% Prim% Defs%
            AstEnv% Store% KApp% KIf% SApp% SIf% SSeq%
            TVar% TStop% Σ% StoBind% 𝓁% Stxξ% Hole% κ%
            InEval% ζ%]))

(define-mixed-unit terms-extra@
  (import)
  (export terms-extra^)
  (inherit [super:terms-extra@
            id lst->list list->lst snoc val? id? stx? stl? proper-stl?]))
