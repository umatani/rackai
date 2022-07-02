#lang racket
(require
 (except-in racket do)
 "../../../nondet.rkt"
 (only-in "../../../term.rkt"            use-terms)
 
 (only-in "../../base/core/terms.rkt"    terms^ #%term-forms)
 (only-in "../../../syntax-sig.rkt"      syntax^)
 (only-in "../../../resolve-sig.rkt"     resolve^)
 (only-in "../../../menv-sig.rkt"        menv^)
 (only-in "../../../mstore-sig.rkt"      mstore^)
 (only-in "../../../phase-sig.rkt"       phase^)

 ;; common in conc/base+set-heap
 (only-in "../resolve-unit.rkt"          resolve@)
 ;; partially reused from conc/base/core
 (rename-in "../../base/core/mstore.rkt" [mstore@ base:mstore@]))
(provide mstore@)

(define-unit mstore/resolve@
  (import (only terms^
                Sym% Stx% Σ% StoBind%)
          (prefix r: (only resolve^
                           resolve id=?))
          (prefix base: (only mstore^
                              init-Σ)))
  (export mstore^)

  (use-terms Sym Stx Σ StoBind)

  (define init-Σ base:init-Σ)

  ;; Set-based Σ

  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  ; bind : Σ Id Nam -> Σ
  (define (bind Σ0 id nam)
    (match-let ([(Σ size tbl) Σ0]
                [(Stx (Sym nam_1) ctx_1) id])
      (Σ size
        (hash-update tbl nam_1
                     (λ (sbss)
                       (for/set ([sbs (in-set sbss)]
                                 #:when (set? sbs))
                         (set-add sbs (StoBind ctx_1 nam))))
                     (λ () (set (set)))))))

  ; lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ))
  (define (lookup-Σ Σ0 nam)
    (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set)))))

  (define resolve r:resolve)
  (define id=?    r:id=?)

  ;; Finite-domain allocation

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (values (string->symbol (format "~a:~a" nam size))
              (Σ (add1 size) tbl))))

  ; alloc-scope : Symbol Σ -> (Values Scp Σ)
  (define (alloc-scope s Σ0)
    (match-let ([(Σ size tbl) Σ0])
      (values (string->symbol (format "~a::~a" s size))
              (Σ (add1 size) tbl)))))

(define-compound-unit mstore@
  (import [t : terms^] [stx : syntax^] [me : menv^] [ph : phase^])
  (export msto)
  (link (([cmsto : mstore^])  base:mstore@    t stx me ph)
        (([r     : resolve^]) resolve@        t stx msto ph)
        (([msto  : mstore^])  mstore/resolve@ t stx r cmsto)))
