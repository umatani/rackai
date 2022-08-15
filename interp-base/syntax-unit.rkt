#lang racket/unit
(require
 (except-in racket set)
 (for-syntax racket)
 "../set.rkt"
 (only-in "../term.rkt" use-terms)
 
 (only-in "../signatures.rkt" syntax^)
 (only-in "../terms.rkt" #%term-forms
          Stx% Null% Pair% Hole% StoBind%
          stx?))

(import)
(export syntax^)

(use-terms Stx Null Pair Hole StoBind)


;; ----------------------------------------
;; Syntax-object operations:

(define (empty-ctx . args) (error "must not be used"))

; zip : ProperStl ProperStl Ctx -> ProperStl
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [((Null) (Null)) (Null)]
    [((Pair stx_left stl_lefts) (Pair stx_right stl_rights))
     (Pair (Stx (Pair stx_left (Pair stx_right (Null))) ctx)
           (zip stl_lefts stl_rights ctx))]))

; unzip : ∀ [A] ProperStl -> (Values ProperStl ProperStl)
(define (unzip stl)
  (match stl
    [(Null) (values (Null) (Null))]
    [(Pair (Stx (Pair stx_left (Pair stx_right (Null))) _) stl_rest)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (Pair stx_left  stl_lefts)
               (Pair stx_right stl_rights)))]))

; in-hole : Stx Stx -> Stl
(define (in-hole . args) (error "must not be used"))

; in-hole-stl : Stl Stx -> Stl
(define (in-hole-stl in-hole stl v)
  (match stl
    [(? stx? stx) (in-hole stx v)]
    [(Pair stx stl) (Pair (in-hole stx v) (in-hole-stl in-hole stl v))]
    [(Hole) v]
    [_ stl]))

; alloc-scope : Symbol -> Scp
(define (alloc-scope s) (gensym s))

;; Recursively strips lexical context from a syntax object
; strip : Stl -> Val
(define (strip stl)
  (match stl
    [(Null) (Null)]
    [(Stx (Pair stx stl) _) (Pair (strip stx) (strip stl))]
    [(Stx x _) x]
    [(Pair stx stl) (Pair (strip stx) (strip stl))]))

;; Adds or cancels a scope
; addremove : Scp Scps -> Scps
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

; subtract : Scps Scps -> Scps
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

; union : Scps Scps -> Scps
(define (union scps1 scps2) (set-union scps1 scps2))

; biggest-subset : Scps (Listof Scps) -> Scps
(define (biggest-subset scps_ref scpss)
  ;(printf "[biggest-subset] ~a ~a\n" scps_ref scpss)
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

; binding-lookup : (Setof StoBind) Scps -> (Option Nam)
(define (binding-lookup sbs scps)
  ;(printf "[binding-lookup] ~a ~a\n" sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))


(define (add . args)        (error "must not be used"))
(define (add-stl . args)    (error "must not be used"))
(define (flip . args)       (error "must not be used"))
(define (flip-stl . args)   (error "must not be used"))
(define (at-phase . args)   (error "must not be used"))
(define (prune . args)      (error "must not be used"))
(define (update-ctx . args) (error "must not be used"))
