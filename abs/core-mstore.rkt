#lang racket
(require
 "../mix.rkt"
 "../example.rkt"

 ;;;; Signatures
 (only-in "../signatures.rkt" syntax^ domain^ menv^ mstore^ run^ debug^)
 (only-in "../conc/base/core/config.rkt" config^)

 ;;;; Units
 ;; reused
 (only-in "../units.rkt"                terms-extra@ env@ menv@ io@)
 (only-in "../conc/base/units.rkt"      cont@ mcont@)
 (only-in "../conc/base/core/units.rkt" config@ syntax@ debug@)
 (only-in "../conc/set/units.rkt"       domain@ store@ #;mstore@ bind@ run@)
 (only-in "../conc/set/core/units.rkt"  eval@ parser@ expander@)

 ;; overridden
 (only-in "../conc/set/units.rkt"       [mstore@ super:mstore@])
 ;; new
 (only-in "alloc.rkt" fin-alloc/mstore@))
(provide run mstore@)

(define-mixed-unit mstore@
  (import)
  (export mstore^)
  (inherit [super:mstore@ init-Σ lookup-Σ update-Σ
                          ;; toriaezu
                          alloc-scope alloc-𝓁]
           [fin-alloc/mstore@ alloc-name #;alloc-scope #;alloc-𝓁]))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-examples run delta core:examples mode))

;; TODO: reftrans
;; メモリを有限にするだけでex-reftransが上手く行かない(2を返す)
;; ことの原因を調査．
;; スコープ関係で何かが起こっている？？
;;  --> alloc-nameのduplicateが原因であることが判明
;;      alloc-scopeのduplicateも生じているが問題にはなっていない
;; にしても，{1, 2} ではなく，{2}だけなのは何故？
;; --> ξ も set-based にする．
;;   さらに，namじゃなくId(Stx)をkeyにすると精度(uniquness)が上がりかつ有限
(module+ test
  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) (x))) 2)) 'eval)

  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) z)) 2)) 'eval))

;; どのドメインが有限？あるいは有限化が必要？
;; その有限性は syntax object が有限であるという仮定に依存しているか？

