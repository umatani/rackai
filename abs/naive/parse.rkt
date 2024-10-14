#lang racket
(require
 (only-in "../../nondet.rkt"          pure)
 (only-in "../../mix.rkt"             define-mixed-unit)
 "../../signatures.rkt"
 (only-in "../../mult/parse-unit.rkt" [parse@ mult:parse@])
 (only-in "domain.rkt"                val-⊤ atom-⊤ stx-⊤))
(provide parse@)

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse parse] parse*))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define ((parse prs prs*) #:phase [ph #f] stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
        (pure val-⊤)
        ((mult:parse prs prs*) #:phase ph stx Σ))))
