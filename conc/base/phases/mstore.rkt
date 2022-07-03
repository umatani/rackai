#lang racket
(require
 (only-in "../../../signatures.rkt"
          syntax^ menv^ bind^ mstore^)
 (only-in "terms.rkt" terms^)

 (only-in "../bind-unit.rkt" bind@)
 ;; partially reused from conc/base/core
 (only-in "../core/mstore.rkt" [mstore@ core:mstore@]))
(provide mstore@)

(define-unit mstore/bind@
  (import
   (only terms^
         Sym% Stx% Σ% StoBind%)
   (only syntax^
         binding-lookup biggest-subset at-phase)
   (prefix b: (only bind^
                    bind resolve id=?))
   (prefix core: (only mstore^
                       init-Σ lookup-Σ alloc-name alloc-scope)))
  (export mstore^)

  (define init-Σ      core:init-Σ)
  (define lookup-Σ    core:lookup-Σ)
  (define alloc-name  core:alloc-name)
  (define alloc-scope core:alloc-scope)

  (define bind    b:bind)
  (define resolve b:resolve)
  (define id=?    b:id=?))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export msto)
  (link (() bind@ msto)
        (([cmsto : mstore^]) core:mstore@)
        (([msto  : mstore^]) mstore/bind@ cmsto)))
