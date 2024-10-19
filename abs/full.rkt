#lang racket/base
(require
 racket/unit
 "../interpreter.rkt"
 "../test/suites.rkt"
 (only-in "../mix.rkt"             define-mixed-unit)
 "../reduction.rkt"
 "../signatures.rkt"
 "../base/full/terms.rkt"

 (only-in "../mult/full/units.rkt"
          io@ cont@ mcont@ syntax@ debug@ domain@ env@ menv@ run@
          eval@ id@ parse@ parser@ expand@ expander@)
 (only-in "alloc.rkt"              store@ mstore@)
 (only-in "phases.rkt"             bind@))
(provide syntax@ main-minus@
         interp)


;; full/set's evaluate already filters out stuck states

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export syntax^ env^ store^ cont^ menv^ mstore^ bind^ id^ mcont^
          run^ debug^)
  (link   syntax@ env@ store@ cont@ menv@ mstore@ bind@ id@ mcont@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         parse@ parser@ eval@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'full   interp))


(module+ test1
  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) (x))) 2)))

  (interp '(let ([z 1])
             ((let-syntax ([x (lambda (stx) #'z)])
                (lambda (z) z)) 2))))

(module+ test2
  (interp '((lambda (f x) (f x))
            (lambda (x) x)
            100)))

;; aとbの引数の名前を別にしたり，zが定義されていないだけで問題は生じない．Why?
;; -->
;; ex-ls-push-rhsのpush-κで内側と外側でconflictする𝓁に継続を重ねて格納していた．
;; そのため，外側にとっての継続であるところの「内側のlet-syntax」をはじめる遷移が
;; 生じていた．
;; 𝓁のフィールドを単にStxにするだけだと equal? になってしまう．
;; 解決方法： 𝓁の equal<%> を stx フィールド の eq? で区別するようにすることで
;;   右辺式が同じであってもレキシカルに異なる binding であれば別の場所を確保する
;;   ことで回避
;; TODO: 仮に今回のような問題点が生じてしまっても，「状態世界」が有限なら
;;   pol-κ した後の「2回目に見かける状態」は無視できるはず．これは，「状態世界」
;;   の一部(とくに κ の一部でもある) Stx 全体を有限化することで根本治療ができるはず．
(module+ test3
  ;; これはbのlambda式のparseの時点で4つ結果が生じるが問題なし．
  (interp '(let-syntax ([z 1])
             (let-syntax ([a (lambda (stx)
                               #'2)])
               (let-syntax ([b (lambda (stx)
                                 stx)])
                 3))))
  ;; こちらは問題あり．上と何が違う？
  (interp '(let-syntax ([z 1])
             (let-syntax ([a (lambda (stx)
                               stx)])
               (let-syntax ([b (lambda (stx)
                                 stx)])
                 3))))
  ;; これもダメ．どうやらevalの結果が同じlambdaだとダメ？
  (interp '(let-syntax ([z 1])
             (let-syntax ([a (lambda ()
                               99)])
               (let-syntax ([b (lambda ()
                                 99)])
                 3)))))
