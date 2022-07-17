#lang racket
(require
 (only-in "../../term.rkt" use-terms)
 (only-in "../../signatures.rkt" store^ mstore^)
 (only-in "../../terms.rkt" terms^ #%term-forms))
(provide fin-alloc/store@ fin-alloc/mstore@)

(define-unit fin-alloc/store@
  (import)
  (export store^)

  (define (init-store    . args) (error "to be implemented"))
  (define (lookup-store  . args) (error "to be implemented"))
  (define (update-store  . args) (error "to be implemented"))
  (define (update-store* . args) (error "to be implemented"))

  ; alloc-loc : Symbol Store -> (Values Loc Store)
  ;   - called only from push-cont
  ;   - a unique lbl is generated for each App and If form during parse
  (define (alloc-loc lbl st)
    (values (string->symbol (format "~a::" lbl)) st))

  ; alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store)
  ;   for eval-time value binding
  (define (alloc-loc* nams st)
    (match nams
      ['() (values '() st)]
      [(list nam1 nams ...)
       (let* ([loc_0 (string->symbol (format "~a:" nam1))])
         (let-values ([(locs_new _) (alloc-loc* nams st)])
           (values (cons loc_0 locs_new) st)))])))


(define-unit fin-alloc/mstore@
  (import (only terms^
                Sym% Stx% 𝓁%))
  (export mstore^)

  (use-terms Sym Stx 𝓁)

  (define (init-Σ        . args) (error "to be implemented"))
  (define (lookup-Σ      . args) (error "to be implemented"))
  (define (update-Σ      . args) (error "to be implemented"))

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ)
    (match-let ([(Stx (Sym nam) _) id])
      (values (string->symbol (format "~a:" nam)) Σ)))

  ; alloc-scope : Symbol Σ -> (Values Scp Σ)
  (define (alloc-scope s Σ)
    (values (string->symbol (format "~a::" s)) Σ))

  ; alloc-𝓁 : Stx Σ -> (Values 𝓁 Σ)
  ;   - called only from push-κ
  ;   - stx is used in abs for ensuring finiteness of the domain
  (define (alloc-𝓁 stx Σ)
    (values ;(𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (𝓁 stx) Σ)))
