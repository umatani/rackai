#lang racket
(provide (all-defined-out))

;(: build-alt-lists : (Listof (Setof A)) -> (Setof (Listof A)))
(define (build-alt-lists alts-list)
  (if (null? alts-list)
      (set '())
      (let ([lsts (build-alt-lists (cdr alts-list))])
        (for*/set ([alt (in-set (car alts-list))]
                   [lst (in-set lsts)])
          (cons alt lst)))))
