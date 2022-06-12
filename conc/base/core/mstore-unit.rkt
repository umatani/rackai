#lang racket/unit
(require racket "../dprint.rkt"
         "../struct-sig.rkt"
         "../syntax-sig.rkt"
         "../menv-sig.rkt"
         "../mstore-sig.rkt")

(import (only struct^ Σ mk-Σ Σ-tbl Stx Sym stobind StoBind-scps tvar)
        (only syntax^
              add biggest-subset binding-lookup)
        (only menv^ extend-ξ))
(export mstore^)

;; ----------------------------------------
;; Expand-time store operations:

; (: init-Σ : -> Σ)
(define (init-Σ) (mk-Σ 0 (make-immutable-hash)))

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
;(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (dprint 'core 'bind "")
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (mk-Σ size (hash-update tbl nam_1
                             (λ (sbs) (set-add sbs (stobind ctx_1 nam)))
                             (λ () (set))))))

;(: lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (dprint 'core 'lookup-Σ "")
  (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

;(: resolve : Id Σ -> Nam)
(define (resolve id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;(: id=? : Id Nam Σ -> Boolean)
(define (id=? id nam Σ) (eq? (resolve id Σ) nam))


;; ----------------------------------------
;; Alloc name & scope helpers for expander:

; (: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (dprint 'core 'alloc-name "")
  (match-let ([(Stx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (mk-Σ (add1 size) tbl))))

; (: alloc-scope : Symbol Σ -> (Values Scp Σ))
(define (alloc-scope s Σ0)
  (dprint 'core 'alloc-scope "")
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "~a::~a" s size))
            (mk-Σ (add1 size) tbl))))

;(: regist-vars : Scp ProperStl ξ Σ -> (Values ProperStl ξ Σ))
(define (regist-vars scp stl ξ Σ)
  (match stl
    ['() (values '() ξ Σ)]
    [(cons (app (λ (stx) stx) id) stl)
     (let*-values ([(stl_reg ξ_1 Σ_1) (regist-vars scp stl ξ Σ)]
                   [(nam_new Σ_2) (alloc-name id Σ_1)]
                   [(id_new) (add id scp)]
                   [(Σ_3) (bind Σ_2 id_new nam_new)]
                   [(ξ_2) (extend-ξ ξ_1 nam_new (tvar id_new))])
       (values (cons id_new stl_reg) ξ_2 Σ_3))]))

