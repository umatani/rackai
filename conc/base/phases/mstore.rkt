#lang racket
(require
 racket/match
 "../../../set.rkt"
 (only-in "../../../dprint.rkt"      dprint)
 (only-in "../../../term.rkt"        use-terms)

 (only-in "terms.rkt"                terms^ #%term-forms)
 (only-in "../../../syntax-sig.rkt"  syntax^)
 (only-in "../../../resolve-sig.rkt" resolve^)
 (only-in "../../../menv-sig.rkt"    menv^)
 (only-in "../../../mstore-sig.rkt"  mstore^)
 (only-in "../../../phase-sig.rkt"   phase^)

 (only-in "../resolve-unit.rkt"      resolve@)
 ;; partially reused from conc/base/core
 (only-in "../core/mstore.rkt"       [mstore@ core:mstore@]))
(provide mstore@)

(define-unit mstore/resolve@
  (import
   (only terms^
         Sym% Stx% Σ% StoBind%)
   (only syntax^
         binding-lookup biggest-subset)
   (prefix r: (only resolve^
                    resolve id=?))
   (only phase^
         at-phase)
   (prefix core: (only mstore^
                       init-Σ lookup-Σ alloc-name alloc-scope)))
  (export mstore^)

  (use-terms Sym Stx Σ StoBind)

  ;; inherited from conc/base/core
  (define init-Σ      core:init-Σ)
  (define lookup-Σ    core:lookup-Σ)
  (define alloc-name  core:alloc-name)
  (define alloc-scope core:alloc-scope)

  ;; Like one-phase `bind`, but extracts scopes at a given phase of
  ;; the identifier
  ;(: bind : Ph Σ Id Nam -> Σ)
  (define (bind ph Σ0 id nam)
    (dprint 'phases 'bind "")
    (match-let ([(Σ size tbl) Σ0]
                [(Stx (Sym nam_1) ctx_1) id])
      (Σ size (hash-update
                tbl nam_1
                (λ (sbs) (set-add sbs (StoBind (at-phase ctx_1 ph) nam)))
                (λ () (set))))))

  ; resolve : Ph Id Σ -> Nam
  (define resolve r:resolve)

  ; id=? : ph Id Nam Σ -> Boolean
  (define id=? r:id=?))

(define-compound-unit mstore@
  (import [t : terms^] [stx : syntax^] [me : menv^] [ph : phase^])
  (export msto)
  (link (([cmsto : mstore^]) core:mstore@     t stx me ph)
        (([r     : resolve^]) resolve@        t stx msto ph)
        (([msto  : mstore^])  mstore/resolve@ t stx r ph cmsto)))
