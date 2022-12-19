#lang racket
(require
 "../set.rkt"
 "../reduction.rkt"
 "../mix.rkt"
 "../interpreter.rkt"
 (only-in "../term.rkt" use-terms)
 ;"../test/suites.rkt"

 (only-in "../signatures.rkt" domain^ syntax^ env^ store^ cont^ eval^
          menv^ mstore^ bind^ mcont^ parser^ expand^ run^ debug^)
 (only-in "../interp-base/full/terms.rkt" #%term-forms
          Var% Fun% App% If% VFun% Null% Pair% Bool% Stx% Sym% Prim% 𝓁% Defs%
          lst->list id? stx-prim?)
 
 (only-in "../units.rkt"                  io@)
 (only-in "../interp-base/units.rkt"      cont@ mcont@)
 (only-in "../interp-base/full/units.rkt"
          [syntax@ super:syntax@] expander@ debug@)
 (only-in "../interp-set/units.rkt"       domain@ env@ menv@ run@)
 (only-in "../interp-set/full/units.rkt"  eval@ parser@ expand@)
 (only-in "alloc.rkt" store@ mstore@ syntax::fin-alloc@ bind@))
(provide syntax@ main-minus@
         interp)

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ empty-ctx zip unzip in-hole in-hole-stl
                          addremove strip subtract union add add-stl
                          at-phase update-ctx prune
                          flip flip-stl]
           [syntax::fin-alloc@ alloc-scope biggest-subset binding-lookup]))

;; full/set's evaluate already filters out stuck states

(define-compound-unit/infer main-minus@
  (import domain^ eval^ parser^ expand^)
  (export syntax^ env^ store^ cont^ menv^ mstore^ bind^ mcont^
          run^ debug^)
  (link   syntax@ env@ store@ cont@ menv@ mstore@ bind@ mcont@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link domain@ main-minus@
         eval@ parser@ expand@))
  (import) (export domain^ run^ debug^))

(define interp (interpreter 'abs:full run delta α ≤a #f))

#;
(define (main [mode 'check])
  (run-suite run delta (suite 'core)   mode α ≤a)
  (run-suite run delta (suite 'phases) mode α ≤a)
  (run-suite run delta (suite 'full)   mode α ≤a))

(module+ test1
  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) (x))) 2)) 'eval)

  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) z)) 2)) 'eval))

(module+ test2
  (run delta '((lambda (f x) (f x))
               (lambda (x) x)
               100) 'eval))

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
  (run delta '(let-syntax ([z 1])
                (let-syntax ([a (lambda (stx)
                                  #'2)])
                  (let-syntax ([b (lambda (stx)
                                    stx)])
                    3))) 'expand)
  ;; こちらは問題あり．上と何が違う？
  (run delta '(let-syntax ([z 1])
                (let-syntax ([a (lambda (stx)
                                  stx)])
                  (let-syntax ([b (lambda (stx)
                                    stx)])
                    3))) 'expand)
  ;; これもダメ．どうやらevalの結果が同じlambdaだとダメ？
  (run delta '(let-syntax ([z 1])
                (let-syntax ([a (lambda ()
                                  99)])
                  (let-syntax ([b (lambda ()
                                    99)])
                    3))) 'expand))

