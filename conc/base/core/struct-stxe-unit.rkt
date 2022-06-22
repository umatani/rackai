#lang racket/unit
(require
 (only-in "../../../struct-common-stxe-sig.rkt" struct-common-stxe^)
 (only-in "struct-stxe-sig.rkt" struct-stxe^))

(import)
(export struct-common-stxe^
        struct-stxe^)

;; specific to conc/base/core
(struct Stxξ (stx ξ) #:transparent #:constructor-name stx&ξ)

(define stx&ξ? Stxξ?)
