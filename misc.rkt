#lang racket/base
(require
 (for-syntax racket/base syntax/parse)
 (only-in racket/list empty? first second rest)
 (only-in "set.rkt"   set ∅ ∪ set-subtract ⊆ set-size set→list set=?)
 "terms.rkt")
(provide require&provide union subtract biggest-subset binding-lookup)

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


;; union : Scps Scps → Scps
(define (union scps scps′) (∪ scps scps′))

;; subtract : Scps Scps → Scps
(define (subtract scps scps′) (set-subtract scps scps′))

; biggest-subset : Scps (Listof Scps) → Scps
(define (biggest-subset scps_ref scpss)
  ;(printf "[biggest-subset] ~a ~a\n" scps_ref scpss)
  (let* ([matching (filter (λ (scps_bind)
                             (⊆ scps_bind scps_ref))
                           scpss)]
         [sorted (sort matching > #:key set-size)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (or (empty? sorted)
            (and (pair? (rest sorted))
                 (= (set-size (first sorted))
                    (set-size (second sorted))))
            (ormap (λ (b) (not (⊆ b (first sorted))))
                   (rest sorted)))
        ∅
        (first sorted))))

; binding-lookup : (Setof StoBind) Scps → (Maybe Nam)
(define (binding-lookup sbs scps)
  ;(printf "[binding-lookup] ~a ~a\n" sbs scps)
  (let ([r (member scps (set→list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))
