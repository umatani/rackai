;; ----------------------------------------
;; The expander:

;; ----------------------------------------
;; Expand-time environment operations:

(: lookup-ξ : ξ Nam -> AllTransform)
(define (lookup-ξ ξ nam) (hash-ref ξ nam (λ () 'not-found)))

(: extend-ξ : ξ Nam AllTransform -> ξ)
(define (extend-ξ ξ nam all-transform) (hash-set ξ nam all-transform))

;; ----------------------------------------
;; Expand-time stack operations:

(: alloc-κ : Θ -> (Values 𝓁 Θ))
(define (alloc-κ θ)
  (match-let ([(Θ size tbl) θ])
    (values (𝓁 (string->symbol (format "k~a" size)))
            (Θ (add1 size) tbl))))

(: lookup-κ : Θ 𝓁 -> κ)
(define (lookup-κ θ 𝓁) (hash-ref (Θ-tbl θ) 𝓁))

(: update-κ : Θ 𝓁 κ -> Θ)
(define (update-κ θ 𝓁 κ)
  (match-let ([(Θ size tbl) θ])
    (Θ size (hash-set tbl 𝓁 κ))))

(: push-κ : Θ κ -> (Values 𝓁 Θ))
(define (push-κ θ κ)
  (let-values ([(𝓁 θ_1) (alloc-κ θ)])
    (values 𝓁 (update-κ θ_1 𝓁 κ))))

;; ----------------------------------------
;; Alloc name & scope helpers for expander:

(: alloc-name : Id Σ -> (Values Nam Σ))
(define (alloc-name id Σ0)
  (match-let ([(GenStx (Sym nam) _) id]
              [(Σ size tbl) Σ0])
    (values (string->symbol (format "~a:~a" nam size))
            (Σ (add1 size) tbl))))

(: alloc-scope : Σ -> (Values Scp Σ))
(define (alloc-scope Σ0)
  (match-let ([(Σ size tbl) Σ0])
    (values (string->symbol (format "scp:~a" size))
            (Σ (add1 size) tbl))))

(define id-kont : Id (GenStx (Sym '#%kont) (empty-ctx)))
(define id-seq  : Id (GenStx (Sym '#%seq)  (empty-ctx)))
(define id-snoc : Id (GenStx (Sym '#%snoc) (empty-ctx)))
(define stx-nil (GenStx '() (empty-ctx)))
