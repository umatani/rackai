#lang racket
(require
 racket/match
 (only-in "../../../term.rkt" use-terms)

 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ phase^)
 (only-in "terms.rkt" terms^ #%term-forms)

 ;; partially reused from conc/base/phases
 (only-in "../phases/syntax.rkt" [syntax@ phases:syntax@]))
(provide syntax@)

(define-unit syntax/super@
  (import
   (only terms^
         Stx% Stxξ% Hole%)
   (only terms-extra^
         atom?)
   (prefix phases: (except syntax^
                           in-hole)))
  (export syntax^)

  (use-terms Stx Stxξ Hole)

  (define empty-ctx      phases:empty-ctx)
  (define stl->seq       phases:stl->seq)
  (define zip            phases:zip)
  (define unzip          phases:unzip)
  (define snoc           phases:snoc)
  (define in-hole-stl    phases:in-hole-stl)
  (define add            phases:add)
  (define add-stl        phases:add-stl)
  (define addremove      phases:addremove)
  (define flip           phases:flip)
  (define flip-stl       phases:flip-stl)
  (define strip          phases:strip)
  [define subtract       phases:subtract]
  (define union          phases:union)
  (define binding-lookup phases:binding-lookup)
  (define biggest-subset phases:biggest-subset)

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
  (export stx phase^)
  (link (([pstx : syntax^]) phases:syntax@)
        (([stx  : syntax^]) syntax/super@ pstx)))
