#lang racket
(require "struct.rkt")
(provide (all-defined-out))

;; ----------------------------------------
;; stx utils

(define (stl->seq stl)
  (match stl
    ['() '()]
    [(cons stx stl) (cons stx (stl->seq stl))]))

(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [('() '()) '()]
    [((cons stx_left stl_lefts) (cons stx_right stl_rights))
     (cons (GenStx `(,stx_left ,stx_right) ctx)
           (zip stl_lefts stl_rights ctx))]))

(define (unzip stl)
  (match stl
    ['() (values '() '())]
    [`(,(GenStx `[,stx_left ,stx_right] _) ,stl_rest ...)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (cons stx_left  stl_lefts)
               (cons stx_right stl_rights)))]))

(define (snoc stl stx)
  (cond
    [(null? stl) (list stx)]
    [(list? stl) (cons (car stl) (snoc (cdr stl) stx))]
    [else (error "no such case")]))

(define (in-hole stx v)
  (match stx
    [(Stxξ stx ξ) (Stxξ (in-hole stx v) ξ)] ; not used
    [(GenStx (? Atom? atom) ctx) (GenStx atom ctx)]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
    [(Hole) v]
    [_ stx]))

(define (in-hole-stl in-hole stl v)
  (match stl
    ['() '()]
    [(? Stx? stx) (in-hole stx v)]
    [(cons stx stl) (cons (in-hole stx v) (in-hole-stl in-hole stl v))]
    [(Hole) v]
    [_ stl]))


;; Adds or cancels a scope
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

;; Recursively strips lexical context from a syntax object
(define (strip stl)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) _) (cons (strip stx) (strip stl))]
    [(GenStx (? Atom? atom) _) atom]
    [(cons stx stl) (cons (strip stx) (strip stl))]))

(define (subtract scps1 scps2) (set-subtract scps1 scps2))

(define (union scps1 scps2) (set-union scps1 scps2))

(define (lookup-Σ Σ0 nam)
  (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

(define (binding-lookup sbs scps)
  (let ([r (member scps (set->list sbs)
                   (λ (scps sb)
                     (set=? scps (StoBind-scps sb))))])
    (and r (StoBind-nam (first r)))))

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
(define (add stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (add stx scp) (add-stl stl scp))
             (set-add ctx scp))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (set-add ctx scp))]))

;(: add-stl : Stl Scp -> Stl)
(define (add-stl stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (add stx scp) (add-stl stl scp))
             (set-add ctx scp))]
    [(GenStx (? Atom? atom) ctx) (GenStx atom (set-add ctx scp))]
    [(cons stx stl) (cons (add stx scp) (add-stl stl scp))]))

;; Pushes flipping a scope down through a syntax object
;(: flip : Stx Scp -> Stx)
(define (flip stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip stx scp) (flip-stl stl scp))
             (addremove scp ctx))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (addremove scp ctx))]))

;(: flip-stl : Stl Scp -> Stl)
(define (flip-stl stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip stx scp) (flip-stl stl scp))
             (addremove scp ctx))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (addremove scp ctx))]
    [(cons stx stl) (cons (flip stx scp) (flip-stl stl scp))]))

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
;(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update tbl nam_1
                          (λ (sbs) (set-add sbs (StoBind ctx_1 nam)))
                          (λ () (set))))))

;(: resolve : Id Σ -> Nam)
(define (resolve id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
