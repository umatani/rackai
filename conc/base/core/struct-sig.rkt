#lang racket
(require
 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "struct-stxe-sig.rkt"            struct-stxe^))
(provide (all-defined-out))

(define-signature struct^
  ((open struct-common^)
   (open struct-stxe^)))

(define-syntax (fuse-struct-imports stx)
  (syntax-case stx (import export)
    [(_ (import sig-i ...) (export sig-e ...) unit-expr)
     #'(unit/new-import-export
        (import struct^ sig-i ...) (export sig-e ...)
        ((sig-e ...)
         unit-expr
         struct-common^ struct-stxe^
         sig-i ...))]))
