#lang racket
(require
 (only-in "../term.rkt" use-terms)
 (only-in "../signatures.rkt" syntax^ store^ mstore^)
 (only-in "../terms.rkt"
          Sym% Stx% 𝓁%
          [#%term-forms tm:#%term-forms])
 (only-in "../config.rkt" config^ [#%term-forms cfg:#%term-forms]))
(provide store::fin-alloc@ mstore::fin-alloc@ syntax::fin-alloc@)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))


(define-unit store::fin-alloc@
  (import)
  (export store^)

  (define (init-store    . args) (error "to be implemented"))
  (define (lookup-store  . args) (error "to be implemented"))
  (define (update-store  . args) (error "to be implemented"))
  (define (update-store* . args) (error "to be implemented"))


  (define all-loc (mutable-seteq))

  ; alloc-loc : Symbol Store -> (Values Loc Store)
  ;   - called only from push-cont
  ;   - a unique lbl is generated for each App and If form during parse
  (define (alloc-loc lbl st)
    (let ([loc (string->symbol (format "~a::" lbl))])
      (if (set-member? all-loc loc)
          (printf "duplicate loc: ~a\n" loc)
          (set-add! all-loc loc))
      (values loc st)))

  ; alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store)
  ;   for eval-time value binding
  (define (alloc-loc* nams st)
    (match nams
      ['() (values '() st)]
      [(list nam1 nams ...)
       (let-values ([(locs _) (alloc-loc* nams st)])
         (let ([loc (string->symbol (format "~a:" nam1))])
           (if (set-member? all-loc loc)
               (printf "duplicate loc: ~a\n" loc)
               (set-add! all-loc loc))
           (values (cons loc locs) st)))])))

(define-unit mstore::fin-alloc@
  (import (only config^
                Σ%))
  (export mstore^)
  (use-terms Sym Stx 𝓁 Σ)

  (define (init-Σ        . args) (error "to be implemented"))
  (define (lookup-Σ      . args) (error "to be implemented"))
  (define (update-Σ      . args) (error "to be implemented"))

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  (define all-name  (mutable-seteq))
  (define all-scope (mutable-seteq))
  (define all-𝓁     (mutable-set))

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (if (or (eq? nam 'z) (eq? nam 'a))
          (let ([nam (string->symbol (format "~a:" nam))])
            (if (set-member? all-name nam)
                (printf "duplicate name: ~a\n" nam)
                (set-add! all-name nam))
            (values nam Σ0))
          (values (string->symbol (format "~a:~a" nam size))
                  (Σ (add1 size) tbl)))))

  ; alloc-𝓁 : Stx Σ -> (Values 𝓁 Σ)
  ;   - called only from push-κ
  ;   - stx is used in abs for ensuring finiteness of the domain
  (define (alloc-𝓁 stx Σ)
    (if (set-member? all-𝓁 stx)
        (printf "duplicate 𝓁: ~a\n" stx)
        (set-add! all-scope stx))
    (values ;(𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (𝓁 stx) Σ)))

(define-unit syntax::fin-alloc@
  (import)
  (export syntax^)

  (define (empty-ctx      . args) (error "to be implemented"))
  (define (zip            . args) (error "to be implemented"))
  (define (unzip          . args) (error "to be implemented"))
  (define (in-hole        . args) (error "to be implemented"))
  (define (in-hole-stl    . args) (error "to be implemented"))
  (define (addremove      . args) (error "to be implemented"))
  (define (strip          . args) (error "to be implemented"))
  (define (add            . args) (error "to be implemented"))
  (define (add-stl        . args) (error "to be implemented"))
  (define (flip           . args) (error "to be implemented"))
  (define (flip-stl       . args) (error "to be implemented"))
  (define (subtract       . args) (error "to be implemented"))
  (define (prune          . args) (error "to be implemented"))
  (define (union          . args) (error "to be implemented"))
  (define (at-phase       . args) (error "to be implemented"))
  (define (update-ctx     . args) (error "to be implemented"))
  (define (binding-lookup . args) (error "to be implemented"))
  (define (biggest-subset . args) (error "to be implemented"))

  ; alloc-scope : Symbol -> Scp
  (define (alloc-scope s) #;(gensym s) s))
