;; ----------------------------------------
;; Drivers

(: init-env : -> Env)
(define (init-env) (make-immutable-hash))

(: init-store : -> Store)
(define (init-store) (Store 0 (make-immutable-hash)))

(: init-ξ : -> ξ)
(define (init-ξ) (make-immutable-hash))

(: init-Σ : -> Σ)
(define (init-Σ) (Σ 0 (make-immutable-hash)))

(: init-Θ : -> Θ)
(define (init-Θ) (Θ 0 (make-immutable-hash)))

(define-helpers (empty-ctx) reader printer)

(: stripper : Stx Σ -> Val)
(define (stripper stx Σ) (strip stx))
