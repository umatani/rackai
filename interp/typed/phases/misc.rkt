#lang typed/racket
(require (rename-in typed/racket/base
                    [read r:read]
                    [eval r:eval]
                    [expand r:expand])
         "types.rkt")
(provide stl->seq zip unzip snoc
         in-hole in-hole-stl
         define-helpers define-runner
         run-ex run-examples run-all-examples)

(include "../misc.rktl")

(: in-hole : Stx Stx -> Stx)
(define (in-hole stx v)
  (match stx
    [(Stxξ ph stx ξ scps) (Stxξ ph (in-hole stx v) ξ scps)] ; added
    [(GenStx (? Atom? atom) ctx) (GenStx atom ctx)]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (in-hole stx v) (in-hole-stl stl v)) ctx)]
    [(Hole) v]
    [_ stx]))
