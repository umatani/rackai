#lang racket
(require
 "terms.rkt")
(provide biggest-subset binding-lookup)

; biggest-subset : Scps (Listof Scps) → Scps
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

; binding-lookup : (Setof StoBind) Scps → (Maybe Nam)
(define (binding-lookup sbs scps)
  ;(printf "[binding-lookup] ~a ~a\n" sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))
