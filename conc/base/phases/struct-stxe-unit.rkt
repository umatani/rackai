#lang racket/unit
(require
 (only-in "../../../struct-common-stxe-sig.rkt" struct-common-stxe^)
 (only-in "struct-stxe-sig.rkt" struct-stxe^))

(import)
(export struct-common-stxe^ struct-stxe^)

;;; updated (ph scps
(struct Stxξ (ph stx ξ scps) #:transparent #:constructor-name stx&ξ)
(define stx&ξ? Stxξ?)
