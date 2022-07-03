#lang racket
(require
 (only-in "../../../signatures.rkt"
          syntax^ menv^ bind^ mstore^)
 (only-in "../../base/phases/terms.rkt" terms^)

 ;; common in conc/base+set-heap
 (only-in "../bind-unit.rkt" bind@)
 ;; partially reused from conc/base+set-heap/core
 (rename-in "../core/mstore.rkt" [mstore@ core:mstore@]))
(provide mstore@)

(define-unit mstore/bind@
  (import
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
