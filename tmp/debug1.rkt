#lang racket
(require debug-scopes
         (for-syntax racket/base
                     debug-scopes)
         (for-meta 2 racket/base
                   debug-scopes))


;; マクロ展開後のコードがマクロ

(define-syntax (foo stx)
  (displayln (+scopes stx))
  (print-full-scopes)
  (displayln (+scopes #'there))
  (print-full-scopes)
  #'(void))

(define-syntax (foo2 stx)
  (define xx (cadr (syntax->list stx)))
  (displayln (+scopes xx))
  (print-full-scopes)
  (define stx2 #'(foo x y))
  (displayln (+scopes stx2))
  (print-full-scopes)
  stx2)

#;(foo2 a)


;; use-siteスコープが複数つく例
;; マクロ展開中に上のフェーズでマクロを使用
