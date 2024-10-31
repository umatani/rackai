#lang racket/base
(require
 (for-syntax racket/base syntax/parse)
 (only-in racket/match   match-define)
 (only-in racket/list    empty? first second rest)
 (only-in "set.rkt"      set ∅ ∪ set-subtract ⊆ set-size set-add
                         set=? set→list for/set in-set)
 "terms.rkt")
(provide require&provide union subtract biggest-subset binding-lookup
         set-of-stobind? lookup-sbs update-sbs)

(begin-for-syntax
  (define-syntax-class require-spec
    (pattern name:id
             #:with rename #'name)
    (pattern [name:id rename:id])))

(define-syntax (require&provide stx)
  (syntax-parse stx
    [(_ [mod spec:require-spec ...] ...)
     #'(begin
         (require (only-in mod spec ...)) ...
         (provide spec.rename ... ...))]
    [(_ mod ...)
     #'(begin
         (require mod ...)
         (provide (all-from-out mod) ...))]))


;;;; Scope-set utilities

;; union : Scps Scps → Scps
(define (union scps scps′) (∪ scps scps′))

;; subtract : Scps Scps → Scps
(define (subtract scps scps′) (set-subtract scps scps′))

;; biggest-subset : Scps (Setof Scps) → Scps
(define (biggest-subset scps scpss)
  (let* ([matchings (filter (λ (scps′) (⊆ scps′ scps))
                            (set→list scpss))]
         [sorted (sort matchings > #:key set-size)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (empty? sorted)
      ∅
      (let ([winner (first sorted)]
            [others (rest  sorted)])
        (if (or (and (not (null? others))
                     (= (set-size winner) (set-size (first others))))
                (ormap (λ (other) (not (⊆ other winner))) others))
          ∅
          winner)))))

;; binding-lookup : (Setof StoBind) Scps → (Maybe Nam)
(define (binding-lookup sbs scps)
  (let ([sbs′ (member scps (set→list sbs)
                      (λ (scps sb) (set=? scps (StoBind-scps sb))))])
    (and sbs′ (StoBind-nam (first sbs′)))))

;;; For use in mult

(define (set-of-stobind? sbs)
  (andmap StoBind? (set→list sbs)))

;; lookup-sbs : (Setof StoBind) Scps → (Setof Nam)
(define (lookup-sbs sbs ctx)
  (define founds (for/list ([sb (in-set sbs)]
                            #:when (set=? (StoBind-scps sb) ctx))
                   (define nams (StoBind-nam sb)) ;; multi
                   nams))
  (cond [(null? founds) #f]
        [(= (length founds) 1) (car founds)]
        [else (error 'lookup-sbs "no such case")]))

;; update-sbs : (Setof StoBind) Scps Nam → (Setof StoBind)
(define (update-sbs sbs ctx nam)
  (for/set ([sb (in-set sbs)])
    (match-define (StoBind scps nams) sb)
    (StoBind scps (if (set=? scps ctx) (set-add nams nam) nams))))
