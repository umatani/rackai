#lang racket
(require
 (except-in racket set do)
 "../../../set.rkt"
 "../../../nondet.rkt"
 (only-in "../../../term.rkt"         use-terms)

 (only-in "../../base/core/terms.rkt" terms^ #%term-forms)
 (only-in "../../../store-sig.rkt"    store^)

 ;; partially reused from conc/base/core
 (rename-in "../../base/core/store-unit.rkt" [store@ base:store@]))
(provide store@)

(define-unit store/super@
  (import (only terms^
                Store%)
          (prefix base: (only store^
                              init-store)))
  (export store^)

  (use-terms Store)

  (define init-store base:init-store)

  ;; Set-based heap

  ;(: lookup-store : Store Loc -> (SetM (U Val Cont))
  (define (lookup-store store loc)
    (lift (hash-ref (Store-tbl store) loc)))

  ;(: update-store : Store Loc (U Val Cont) -> Store)
  (define (update-store store0 loc u)
    (Store (Store-size store0)
           (hash-update (Store-tbl store0) loc
                        (λ (old) (set-add old u)) (set))))

  ;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
  (define (update-store* store0 locs us)
    (Store (Store-size store0)
           (foldl (λ (loc u tbl)
                    (hash-update tbl loc (λ (old) (set-add old u)) (set)))
                  (Store-tbl store0) locs us)))


  ;; Finite-domain allocation

  ;(: alloc-loc : Store -> (Values Loc Store))
  (define (alloc-loc store0)
    (let ([size (Store-size store0)])
      (values (string->symbol (format "l~a" size))
              (Store (add1 size) (Store-tbl store0)))))

  ;; for eval-time value binding
  ;(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
  (define (alloc-loc* nams store0)
    (match nams
      ['() (values '() store0)]
      [(list nam1 nams ...)
       (let* ([size (Store-size store0)]
              [loc_0 (string->symbol (format "~a:~a" nam1 size))])
         (let-values ([(locs_new store_new)
                       (alloc-loc* nams
                                   (Store (add1 size) (Store-tbl store0)))])
           (values (cons loc_0 locs_new) store_new)))])))

(define-compound-unit store@
  (import [t : terms^])
  (export sto)
  (link (([bsto : store^]) base:store@  t)
        (([sto  : store^]) store/super@ t bsto)))

