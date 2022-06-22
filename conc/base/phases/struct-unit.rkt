#lang racket
(require
 (only-in "../../../struct-common-sig.rkt"      struct-common^)
 (only-in "../../../struct-common-stxe-sig.rkt" struct-common-stxe^)
 (only-in "struct-stxe-sig.rkt"                 struct-stxe^)
 (only-in "struct-sig.rkt" struct^)

 (only-in "../../../struct-common-unit.rkt"     struct-common@)
 (only-in "struct-stxe-unit.rkt"                struct-stxe@))
(provide struct@)

;; Mixin
(define-unit/new-import-export struct@
  (import) (export struct^)
  ((struct-common^ struct-stxe^)
   (compound-unit
    (import)
    (export sc se)
    (link (([se : struct-stxe^] [scse : struct-common-stxe^]) struct-stxe@)
          (([sc : struct-common^]) struct-common@ scse)))))
