#lang racket/unit
(require (except-in racket set)
         "../set.rkt" "../dprint.rkt"
         "../struct-sig.rkt"
         "../syntax-sig.rkt")

(import (only struct^
              Stx stx stx? Stxξ stx&ξ Sym sym atom? Hole
              Σ mk-Σ Σ-tbl StoBind-nam StoBind-scps stobind))
(export syntax^)

(define (empty-ctx) (set))

;; ----------------------------------------
;; stx utils

;(: stl->seq : Stl -> (Listof Stx))
(define (stl->seq stl)
  (match stl
    ['() '()]
    [(cons stx0 stl) (cons stx0 (stl->seq stl))]))

;(: zip : ProperStl ProperStl Ctx -> ProperStl)
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [('() '()) '()]
    [((cons stx_left stl_lefts) (cons stx_right stl_rights))
     (cons (stx `(,stx_left ,stx_right) ctx)
           (zip stl_lefts stl_rights ctx))]))

;(: unzip : ∀ [A] ProperStl -> (Values ProperStl ProperStl))
(define (unzip stl)
  (match stl
    ['() (values '() '())]
    [`(,(Stx `[,stx_left ,stx_right] _) ,stl_rest ...)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (cons stx_left  stl_lefts)
               (cons stx_right stl_rights)))]))

;(: snoc (-> ProperStl Stx ProperStl))
(define (snoc stl stx0)
  (cond
    [(null? stl) (list stx0)]
    [(list? stl) (cons (car stl) (snoc (cdr stl) stx0))]
    [else (error "no such case")]))

;(: in-hole-stl : Stl Stx -> Stl)
(define (in-hole stx0 v)
  (match stx0
    [(Stxξ stx0 ξ) (stx&ξ (in-hole stx0 v) ξ)] ; not used
    [(Stx (? atom? atom) ctx) (stx atom ctx)]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (in-hole stx0 v) (in-hole-stl in-hole stl v)) ctx)]
    [(Hole) v]
    [_ stx0]))

;(: in-hole-stl : Stl Stx -> Stl)
(define (in-hole-stl in-hole stl v)
  (match stl
    ['() '()]
    [(? stx? stx0) (in-hole stx0 v)]
    [(cons stx0 stl) (cons (in-hole stx0 v) (in-hole-stl in-hole stl v))]
    [(Hole) v]
    [_ stl]))


;; Adds or cancels a scope
;(: addremove : Scp Scps -> Scps)
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

;; Recursively strips lexical context from a syntax object
;(: strip : Stl -> Val)
(define (strip stl)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) _) (cons (strip stx0) (strip stl))]
    [(Stx (? atom? atom) _) atom]
    [(cons stx0 stl) (cons (strip stx0) (strip stl))]))

;(: subtract : Scps Scps -> Scps)
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

;(: union : Scps Scps -> Scps)
(define (union scps1 scps2) (set-union scps1 scps2))

;(: binding-lookup : (Setof StoBind) Scps -> (Option Nam))
(define (binding-lookup sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))

;(: biggest-subset : Scps (Listof Scps) -> Scps)
(define (biggest-subset scps_ref scpss)
  (let* ([matching (filter (λ (scps_bind)
                             (subset? scps_bind scps_ref))
                           scpss)]
         [sorted (sort matching > #:key set-count)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (or (empty? sorted)
            (and (pair? (rest sorted))
                 (= (set-count (first sorted))
                    (set-count (second sorted))))
            (ormap (λ (b) (not (subset? b (first sorted))))
                   (rest sorted)))
        (set)
        (first sorted))))

;; ----------------------------------------
;; Syntax-object operations:

;; Simply pushes scopes down through a syntax object
;(: add : Stx Scp -> Stx)
(define (add stx0 scp)
  (match stx0
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (add stx0 scp) (add-stl stl scp))
          (set-add ctx scp))]
    [(Stx (? atom? atom) ctx)
     (stx atom (set-add ctx scp))]))

;(: add-stl : Stl Scp -> Stl)
(define (add-stl stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (add stx0 scp) (add-stl stl scp))
          (set-add ctx scp))]
    [(Stx (? atom? atom) ctx) (stx atom (set-add ctx scp))]
    [(cons stx0 stl) (cons (add stx0 scp) (add-stl stl scp))]))

;; Pushes flipping a scope down through a syntax object
;(: flip : Stx Scp -> Stx)
(define (flip stx0 scp)
  (match stx0
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (flip stx0 scp) (flip-stl stl scp))
          (addremove scp ctx))]
    [(Stx (? atom? atom) ctx)
     (stx atom (addremove scp ctx))]))

;(: flip-stl : Stl Scp -> Stl)
(define (flip-stl stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (flip stx0 scp) (flip-stl stl scp))
          (addremove scp ctx))]
    [(Stx (? atom? atom) ctx)
     (stx atom (addremove scp ctx))]
    [(cons stx0 stl) (cons (flip stx0 scp) (flip-stl stl scp))]))

;; ----------------------------------------
;; Constants:

(define id-kont (stx (sym '#%kont) (empty-ctx)))
(define id-seq (stx (sym '#%seq)  (empty-ctx)))
(define id-snoc (stx (sym '#%snoc) (empty-ctx)))
(define stx-nil (stx '() (empty-ctx)))

