#lang racket
(require
 racket/match
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt" terms-extra^ syntax^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt"        [syntax@ super:syntax@])
 (only-in "../phases/units.rkt" [syntax@ phases:syntax@]))
(provide syntax@)

(define-unit syntax/super@
  (import
   (only terms^       Stx% Stxξ% Hole%)
   (only terms-extra^ atom?)
   (prefix super: (only syntax^
                        stl->seq zip unzip snoc in-hole-stl
                        addremove strip subtract union
                        binding-lookup biggest-subset))
   (tag p (prefix phases: (only syntax^
                                empty-ctx add add-stl flip flip-stl
                                at-phase update-ctx prune))))
  (export syntax^)

  (use-terms Stx Stxξ Hole)

  (define stl->seq       super:stl->seq)
  (define zip            super:zip)
  (define unzip          super:unzip)
  (define snoc           super:snoc)
  (define in-hole-stl    super:in-hole-stl)
  (define addremove      super:addremove)
  (define strip          super:strip)
  (define subtract       super:subtract)
  (define union          super:union)
  (define binding-lookup super:binding-lookup)
  (define biggest-subset super:biggest-subset)

  (define empty-ctx      phases:empty-ctx)
  (define add            phases:add)
  (define add-stl        phases:add-stl)
  (define flip           phases:flip)
  (define flip-stl       phases:flip-stl)
  (define at-phase       phases:at-phase)
  (define update-ctx     phases:update-ctx)
  (define prune          phases:prune)

  ; in-hole : Stx Stx -> Stx
  (define (in-hole stx v)
    (match stx
      [(Stxξ ph stx ξ) (Stxξ ph (in-hole stx v) ξ)] ; remove scps
      [(Stx (? atom? atom) ctx) (Stx atom ctx)]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx])))

(define-compound-unit/infer syntax@
  (import terms^ terms-extra^)
  (export s)
  (link (([ss : syntax^]) super:syntax@)
        (([ps : syntax^]) phases:syntax@)
        (([s  : syntax^]) syntax/super@ (tag p ps) ss)))
