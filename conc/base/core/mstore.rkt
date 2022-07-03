#lang racket
(require
 racket/match
 "../../../set.rkt"
 (only-in "../../../term.rkt" use-terms)
 (only-in "../../../dprint.rkt" dprint)

 (only-in "../../../signatures.rkt"
          syntax^ menv^ resolve^ mstore^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../resolve-unit.rkt" resolve@))
(provide mstore@)

(define-unit mstore/resolve@
  (import (only terms^
                Sym% Stx% Σ% StoBind%)
          (only syntax^
                add biggest-subset binding-lookup)
          (only menv^
                extend-ξ)
          (prefix r: (only resolve^
                           resolve id=?)))
  (export mstore^)

  (use-terms Sym Stx Σ StoBind)


  ;; ----------------------------------------
  ;; Expand-time store operations:

  ; init-Σ : -> Σ
  (define (init-Σ) (Σ 0 (make-immutable-hash)))

  ;; Add a binding using the name and scopes of an identifier, mapping
  ;; them in the store to a given name
  ; bind : Σ Id Nam -> Σ
  (define (bind Σ0 id nam)
    (dprint 'core 'bind "")
    (match-let ([(Σ size tbl) Σ0]
                [(Stx (Sym nam_1) ctx_1) id])
      (Σ size (hash-update tbl nam_1
                            (λ (sbs) (set-add sbs (StoBind ctx_1 nam)))
                            (λ () (set))))))

  ;; lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ)
  (define (lookup-Σ Σ0 nam)
    (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

  ; resolve : Id Σ -> Nam
  (define resolve r:resolve)

  ;; id=? : Id Nam Σ -> Boolean
  (define id=? r:id=?)


  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (dprint 'core 'alloc-name "")
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (values (string->symbol (format "~a:~a" nam size))
              (Σ (add1 size) tbl))))

  ; alloc-scope : Symbol Σ -> (Values Scp Σ)
  (define (alloc-scope s Σ0)
    (dprint 'core 'alloc-scope "")
    (match-let ([(Σ size tbl) Σ0])
      (values (string->symbol (format "~a::~a" s size))
              (Σ (add1 size) tbl)))))

(define-compound-unit/infer mstore@
  (import terms^ syntax^ menv^)
  (export mstore^)
  (link   resolve@ mstore/resolve@))

#;
(define-compound-unit mstore@
  (import [t : terms^] [stx : syntax^] [me : menv^])
  (export msto)
  (link (([r : resolve^]) resolve@ t stx msto)
        (([msto : mstore^]) mstore/resolve@ t stx me r)))
