;; Adds or cancels a scope
(: addremove : Scp Scps -> Scps)
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

;; Recursively strips lexical context from a syntax object
(: strip : Stl -> Val)
(define (strip stl)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) _) (cons (strip stx) (strip stl))]
    [(GenStx (? Atom? atom) _) atom]
    [(cons stx stl) (cons (strip stx) (strip stl))]))

(: subtract : Scps Scps -> Scps)
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

(: union : Scps Scps -> Scps)
(define (union scps1 scps2) (set-union scps1 scps2))

(: lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (hash-ref (Σ-tbl Σ0) nam (λ () (ann (set) (Setof StoBind)))))

(: binding-lookup : (Setof StoBind) Scps -> (Option Nam))
(define (binding-lookup sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ ([scps : Scps] [sb : StoBind])
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))

(: biggest-subset : Scps (Listof Scps) -> Scps)
(define (biggest-subset scps_ref scpss)
  (let* ([matching : (Listof Scps)
                   (filter (λ ([scps_bind : Scps])
                             (subset? scps_bind scps_ref))
                           scpss)]
         [sorted : (Listof Scps)
                 ((inst sort Scps Index)
                  matching > #:key set-count)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    (if (or (empty? sorted)
            (and (pair? (rest sorted))
                 (= (set-count (first sorted))
                    (set-count (second sorted))))
            (ormap (λ ([b : Scps]) (not (subset? b (first sorted))))
                   (rest sorted)))
        (set)
        (first sorted))))
