#lang racket

;; signatureの一部を変更したい．古いsignatureを見ていたunitと
;; 新しいsignatureを見る unit を compose できるか？

(define-signature old^
  ((struct StxE (a b) #:constructor-name stx&e)))

(define-signature new^ extends old^
  ((struct StxE (a c) #:constructor-name stx&e)))
