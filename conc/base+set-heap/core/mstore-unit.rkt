#lang racket/unit
(require (except-in racket do)
         "../../../nondet.rkt"
         
         "../../../struct-sig.rkt"
         "../../../syntax-sig.rkt"
         "../../../mstore-sig.rkt")

(import (only struct^
              Stx Sym Σ mk-Σ Σ-tbl StoBind-scps stobind)
        (only syntax^
              biggest-subset binding-lookup)
        (prefix base: (only mstore^ init-Σ)))
(export mstore^)


(define init-Σ         base:init-Σ)

;; Set-based Σ

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
;(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (mk-Σ size
           (hash-update tbl nam_1
                        (λ (sbss)
                          (for/set ([sbs (in-set sbss)]
                                    #:when (set? sbs))
                            (set-add sbs (stobind ctx_1 nam))))
                        (λ () (set (set)))))))

;(: lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set)))))

;(: resolve : Id Σ -> (SetM Nam)
(define (resolve id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list
                               (car (do (lookup-Σ Σ0 nam)))))]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           [scps_biggests (map (λ (scpss) (biggest-subset ctx scpss))
                               scpsss)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      (lift (if (null? nam_biggests)
                (set nam)
                (list->set nam_biggests))))))

;(: id=? : Id Nam Σ -> Boolean)
(define (id=? id nam Σ) (subset? (set nam) (car (do (resolve id Σ)))))

;; Finite-domain allocation

; (: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (match-let ([(Stx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (mk-Σ (add1 size) tbl))))

; (: alloc-scope : Symbol Σ -> (Values Scp Σ))
(define (alloc-scope s Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "~a::~a" s size))
            (mk-Σ (add1 size) tbl))))
