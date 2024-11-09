#lang racket/base
(require
 racket/unit
 "../interpreter.rkt"
 "../signatures.rkt"
 "../test/suites.rkt"
 (only-in "../mult/core/units.rkt"
          common@ misc@ bind@ io@ expand@ expander@ syntax@ domain@
          env@ menv@ run@ eval@ evaluator@ parse@ parser@)
 (only-in "alloc.rkt" store@ mstore@))
(provide main-minus@ interp)


;;;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ expand^ parser^)
  (export common^ misc^ syntax^ env^ store^ evaluator^ menv^ mstore^ bind^
          run^)
  (link   common@ misc@ syntax@ env@ store@ evaluator@ menv@ mstore@ bind@
          expander@ io@ run@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link main-minus@
         domain@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp))



;; alloc-nameのduplicate(衝突)への対策
;; にしても，{1, 2} ではなく，{2}だけなのは何故？
;; --> ξ, env の両方を set-based にする．
;; --> これで{1, 2}の両方が入る．
;; TODO: さらに，namじゃなくId(Stx)をeq?なkeyにして精度(uniquness)が上がり
;; baseと同じ {1} だけに戻るはず．

(module+ test1
  (interp '(let ([z 1])
              ((let-syntax ([x (lambda (stx) #'z)])
                 (lambda (z) (x))) 2)))

  (interp '(let ([z 1])
              ((let-syntax ([x (lambda (stx) #'z)])
                 (lambda (z) z)) 2))))

;; alloc-scope の duplicate による問題への対策
;; 別のlambda式の x が duplicate によって ambiguous 扱いとなる．
;; --> ambiguous の原因となっているすべての候補を近似的にresolve結果とする．
;;     resolveの曖昧さによってunboundとなるようなケースは errorではなく結果
;;     の候補から取り除く．これで expand は通過
;; parse時にあらためてresolveでillegalなものも含め「全ての」resolve結果の
;; 組み合わせが生成されてしまう．
;; --> parseを通過はするので，やはりeval時のreferenceでunboundな場合を
;;     結果から削除
;;
;; TODO: さらに，Scopeを単純な nam で生成するのではなく，Stxを元に生成すれば
;; 精度が上がるはず(この例だと2つのbinding xで別々のscopeを生成)

(module+ test2
  (interp '((lambda (f x) (f x))
            (lambda (x) x)
            100)))

;; resolveに関連して問題が発生している <- 上のTODOのようなalloc-scopeの解決
;; で十分？ctx部分の表現の工夫(PRO時点)はどう影響？
