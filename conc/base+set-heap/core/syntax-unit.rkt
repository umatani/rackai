#lang racket
(require "../../base/nondet.rkt"
         "../../base/struct-sig.rkt"
         "../../base/syntax-sig.rkt")
(provide syntax@)

(define-unit syntax@
  (import (only struct^
                Stx Sym Σ mk-Σ Σ-tbl StoBind-scps stobind)
          (prefix base: syntax^))
  (export syntax^)

  (define empty-ctx      base:empty-ctx)
  (define stl->seq       base:stl->seq)
  (define zip            base:zip)
  (define unzip          base:unzip)
  (define snoc           base:snoc)
  (define in-hole        base:in-hole)
  (define in-hole-stl    base:in-hole-stl)
  (define addremove      base:addremove)
  (define strip          base:strip)
  (define subtract       base:subtract)
  (define union          base:union)
  (define binding-lookup base:binding-lookup)
  (define biggest-subset base:binding-lookup)

  (define add            base:add)
  (define add-stl        base:add-stl)
  (define flip           base:flip)
  (define flip-stl       base:flip-stl)
  ;(define bind           base:bind)
  ;(define lookup-Σ       base:lookup-Σ)
  ;(define resolve        base:resolve)
  ;(define id=?           base:id=?)


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
  )
