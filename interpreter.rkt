#lang racket/base
(require
 (only-in racket/match   match-λ match-define)
 (only-in racket/list    first)
 (only-in racket/sandbox sandbox-make-code-inspector make-evaluator)
 (only-in "set.rkt" set)
 "terms.rkt")
(provide raw-eval interpreter reset-outcomes get-outcomes show-outcomes)

(define r:eval
  ;; gain access to local-expand etc.
  (parameterize ([sandbox-make-code-inspector (λ () (current-code-inspector))])
      (make-evaluator 'racket
                      #:requires '((for-syntax racket/list)))))

;; raw-eval : Sexp → (Setof Val)
;;   Host evaluator for checking outcomes
(define (raw-eval form)
  (define r→v
    (match-λ
     [(? null?)      (Null)]
     [(? boolean? b) (Bool b)]
     [(? number? n)  (Num n)]
     [(? symbol? s)  (Sym s)]
     [(cons a d)     (Pair (r→v a) (r→v d))]))
  (set (r→v (first (call-with-values
                    (λ () (r:eval form))
                    (λ vs vs))))))

(struct interp (run δ α ≤ₐ outcomes)
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

;; reset-outcomes : Interp → Void
(define (reset-outcomes interpreter)
  (hash-clear! (interp-outcomes interpreter))
  (hash-set*! (interp-outcomes interpreter)
              'exact   0
              'inexact 0
              'unsound 0
              'fail    0))

;; get-outcomes : Interp → (Listof (Cons Symbol Nat))
(define (get-outcomes interpreter)
  (interp-outcomes interpreter))

;; show-outcomes : Interp → Void
(define (show-outcomes interpreter)
  (define outcomes (get-outcomes interpreter))
  (define e (hash-ref outcomes 'exact))
  (define i (hash-ref outcomes 'inexact))
  (define u (hash-ref outcomes 'unsound))
  (define f (hash-ref outcomes 'fail))

  (printf "OK : ~a (~a exact)\n" (+ e i) e)
  (printf "NG : ~a (~a fail)\n"  (+ u f) f))
