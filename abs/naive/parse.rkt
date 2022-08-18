#lang racket
(require
 "../../nondet.rkt"
 "../../mix.rkt"
 (only-in "../../signatures.rkt" domain^ syntax^ menv^ bind^ parse^)
 (only-in "../../interp-set/parse-unit.rkt" [parse@ set:parse@])
 (only-in "domain.rkt" val-⊤ atom-⊤ stx-⊤))
(provide parse@)

(define-mixed-unit parse@
  (import)
  (export parse^)
  (inherit (set:parse@ [set:parse parse] parse*))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define ((parse prs prs*) #:phase [ph #f] stx Σ)
    (if (or (equal? stx val-⊤)
            (equal? stx atom-⊤)
            (equal? stx stx-⊤))
        (pure val-⊤)
        ((set:parse prs prs*) #:phase ph stx Σ))))
