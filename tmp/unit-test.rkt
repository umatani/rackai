#lang racket

(define-signature foo^
  ((define-values (foo^) 'foo)
   (define-syntaxes (m) (λ (stx) #'foo^))))

(define-signature bar^ extends foo^
  ((define-values (y) 'y)
   (define-syntaxes (bar^) (λ (stx) #'(m)))))

(invoke-unit (unit (import foo^) (export)
               (m))
             ;(import foo^)
             (import bar^)
             )

(invoke-unit (unit (import bar^) (export)
               (bar^))
             (import bar^))

(define-signature baz^
  ((open bar^)
   (define-values (z) 'z)
   (define-syntaxes (m3) (λ (stx) #'(m)))
   (define-syntaxes (m4) (λ (stx) #'(bar^)))
   (define-syntaxes (m5) (λ (stx) #'foo^))
   (define-syntaxes (m6) (λ (stx) #'y))
   ))

(invoke-unit (unit (import baz^) (export)
               (list (m3) (m4) (m5) (m6)))
             (import baz^))

;; unit内部で define-signatureできるのか？ --> できない
(define-unit outer@
  (import)
  (export)
  (define-signature inner^ (-->)))

(invoke-unit outer@)

;(define-signature inner^ (-->))


;; openで組み合わせたsignatureをinstantiate

(define-signature A^
  (A))

(define-signature comp^
  ((open A^)
   (define-values (x) (+ A 1))))

(invoke-unit (compound-unit
              (import)
              (export)
              (link (([c : comp^]) (unit (import) (export comp^)
                                     (define A 100)
                                     'DEFINE-COMP))
                    (() (unit (import comp^) (export)
                          (list x)) c)
                    (() (unit (import) (export)
                          'EMPTY!)))))
