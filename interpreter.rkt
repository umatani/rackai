#lang racket
(require
 racket/sandbox
 "terms.rkt")
(provide raw-eval interpreter reset-results get-results show-results)

(define r:eval (current-eval)
  #;
  (make-evaluator
   'racket
   #:requires '(racket/list (for-syntax racket/list))))


;; raw-eval : Sexp → (Setof Val)
;;   Host evaluater for checking results
(define (raw-eval form)
  (define r→v
    (match-λ
     [(? null?)      (Null)]
     [(? boolean? b) (Bool b)]
     [(? number? n)  (Num n)]
     [(? symbol? s)  (Sym s)]
     [(cons a d)     (Pair (r→v a) (r→v d))]))
  (set (r→v (first (call-with-values
                    (λ () (call-with-trusted-sandbox-configuration
                           (λ () (r:eval form))))
                    (λ vs vs))))))

(struct interp (run δ α ≤ₐ results)
  #:property
  prop:procedure
  ;; self : Sexp → (U Val (Setof Val))
  (λ (self form
           #:mode     [mode      'eval]
           #:check    [reference #f])
    (match-define (interp run δ α ≤ₐ rslts) self)
    (define v (run δ form mode))

    (if (and reference (eq? mode 'eval))
      (with-handlers ([exn:fail? (λ (_)
                                   (hash-update! rslts 'fail add1)
                                   'fail)])
        (define ref-eval (if (boolean? reference)
                           raw-eval
                           reference))
        (let ([r (with-handlers
                   ([exn:fail?
                     (λ (e)
                       (printf "error in reference model: ~a\n" e))])
                   (ref-eval form))]
              [a (α v)])
          (cond
            [(and (≤ₐ r a)
                  (≤ₐ a r)) (hash-update! rslts 'exact   add1) 'exact]
            [(≤ₐ r a)       (hash-update! rslts 'inexact add1) 'inexact]
            [else           (hash-update! rslts 'unsound add1) 'unsound])))
      v)))


;; interpreter : Symbol (δ Sexp Symbol → (U Val (Setof Val))) δ
;;               (→ Val (Setof Val)) (→ (Setof Val) (Setof Val) Boolean)
;;                 → (U Val (Setof Val))
(define (interpreter run δ α ≤ₐ)
  (interp run δ α ≤ₐ (make-hasheq '((exact   . 0)
                                    (inexact . 0)
                                    (unsound . 0)
                                    (fail    . 0)))))

;; reset-results : Interp → Void
(define (reset-results interpreter)
  (hash-clear! (interp-results interpreter))
  (hash-set*! (interp-results interpreter)
              'exact   0
              'inexact 0
              'unsound 0
              'fail    0))

;; get-results : Interp → (Listof (Cons Symbol Nat))
(define (get-results interpreter)
  (interp-results interpreter))

;; show-results : Interp → Void
(define (show-results interpreter)
  (define results (get-results interpreter))
  (define e (hash-ref results 'exact))
  (define i (hash-ref results 'inexact))
  (define u (hash-ref results 'unsound))
  (define f (hash-ref results 'fail))

  (printf "OK : ~a (~a exact)\n" (+ e i) e)
  (printf "NG : ~a (~a fail)\n"  (+ u f) f))
