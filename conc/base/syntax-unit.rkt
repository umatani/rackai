#lang racket/unit
(require
 (except-in racket set)
 "../../set.rkt"
 (only-in "../../term.rkt" use-terms)
 
 (only-in "../../signatures.rkt" terms-extra^ syntax^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import (only terms^
              Stx% StoBind% Hole%)
        (only terms-extra^
              stx? atom?))
(export syntax^)

(use-terms Stx StoBind Hole)


;; ----------------------------------------
;; stx utils

(define (empty-ctx . args) (error "must not be used"))

; stl->seq : Stl -> (Listof Stx)
(define (stl->seq stl)
  (match stl
    ['() '()]
    [(cons stx stl) (cons stx (stl->seq stl))]))

; zip : ProperStl ProperStl Ctx -> ProperStl
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [('() '()) '()]
    [((cons stx_left stl_lefts) (cons stx_right stl_rights))
     (cons (Stx `(,stx_left ,stx_right) ctx)
           (zip stl_lefts stl_rights ctx))]))

; unzip : ∀ [A] ProperStl -> (Values ProperStl ProperStl)
(define (unzip stl)
  (match stl
    ['() (values '() '())]
    [`(,(Stx `[,stx_left ,stx_right] _) ,stl_rest ...)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (cons stx_left  stl_lefts)
               (cons stx_right stl_rights)))]))

; snoc : ProperStl Stx -> ProperStl
(define (snoc stl stx)
  (cond
    [(null? stl) (list stx)]
    [(list? stl) (cons (car stl) (snoc (cdr stl) stx))]
    [else (error "no such case")]))

; in-hole : Stx Stx -> Stl
(define (in-hole . args) (error "must not be used"))

; in-hole-stl : Stl Stx -> Stl
(define (in-hole-stl in-hole stl v)
  (match stl
    ['() '()]
    [(? stx? stx) (in-hole stx v)]
    [(cons stx stl) (cons (in-hole stx v) (in-hole-stl in-hole stl v))]
    [(Hole) v]
    [_ stl]))

;; Adds or cancels a scope
; addremove : Scp Scps -> Scps
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

;; Recursively strips lexical context from a syntax object
; strip : Stl -> Val
(define (strip stl)
  (match stl
    ['() '()]
    [(Stx (cons stx stl) _) (cons (strip stx) (strip stl))]
    [(Stx (? atom? atom) _) atom]
    [(cons stx stl) (cons (strip stx) (strip stl))]))

; subtract : Scps Scps -> Scps
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

; union : Scps Scps -> Scps
(define (union scps1 scps2) (set-union scps1 scps2))

; binding-lookup : (Setof StoBind) Scps -> (Option Nam)
(define (binding-lookup sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))

; biggest-subset : Scps (Listof Scps) -> Scps
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

(define (add . args)        (error "must not be used"))
(define (add-stl . args)    (error "must not be used"))
(define (flip . args)       (error "must not be used"))
(define (flip-stl . args)   (error "must not be used"))
(define (at-phase . args)   (error "must not be used"))
(define (prune . args)      (error "must not be used"))
(define (update-ctx . args) (error "must not be used"))