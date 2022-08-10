#lang racket
(require
 "../set.rkt"
 "../reduction.rkt"
 "../mix.rkt"
 (only-in "../term.rkt" use-terms)
 "../example.rkt"

 (only-in "../signatures.rkt" terms-extra^ syntax^ env^ store^ cont^ eval^
          domain^ menv^ mstore^ bind^ parser^ expand^ run^ debug^)
 (only-in "../conc/base/full/config.rkt" config^ #%term-forms)
 (only-in "../terms.rkt"
          Var% Fun% App% If% VFun% Null% Pair% Bool% Stx% Sym% Prim% 𝓁% Defs%
          lst->list id? stx-prim?)
 
 (only-in "../units.rkt"                 terms-extra@ io@)
 (only-in "../conc/base/units.rkt"       cont@ mcont@)
 (only-in "../conc/base/full/units.rkt"
          [syntax@ super:syntax@] config@ expander@ debug@)
 (only-in "../conc/set/units.rkt"        env@ domain@ menv@ run@)
 (only-in "../conc/set/full/units.rkt"   [eval@ set:eval@] parser@ expand@)
 (only-in "../conc/set/full/eval.rkt"    [--> set:-->])
 (only-in "alloc.rkt" store@ mstore@ syntax::fin-alloc@ bind@))
(provide syntax@ run delta α ≤a)

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ empty-ctx zip unzip in-hole in-hole-stl
                          addremove strip subtract union add add-stl
                          at-phase update-ctx prune
                          flip flip-stl]
           [syntax::fin-alloc@ alloc-scope biggest-subset binding-lookup]))

;; Revise evaluate to filter out stuck states

;; --> : State -> (Setof State)
(define-reduction (--> delta ==>) #:super (set:--> delta ==>)
  #:within-signatures [(only terms-extra^
                             val?)
                       (only config^
                             SApp% SIf% KApp% KIf% AstEnv% Stxξ% Σ% Σ*% ζ%
                             TVar% TStop% InExpand%)
                       (only syntax^
                             add flip prune union alloc-scope)
                       (only env^
                             init-env lookup-env extend-env)
                       (only store^
                             alloc-loc* lookup-store update-store*)
                       (only cont^
                             push-cont)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             alloc-name alloc-𝓁 lookup-Σ update-Σ)
                       (only bind^
                             bind resolve)
                       (only parser^
                             parse)]

  ;; local expand
  [`(,(SApp lbl `(,ph ,maybe-scp_i ,ξ)
            `(,(Prim 'local-expand _)
              ,(? Stx? stx) ,val_contextv ,val_idstops) '())
     ,cont ,store ,(and Σ*_0 (Σ* Σ _ _)))
   #:with ξ_unstops := (make-immutable-hash
                         (map (λ (p) (cons (car p) (unstop (cdr p))))
                              (hash->list ξ)))
   #:with nams_stop <- (resolve* ph (lst->list val_idstops) Σ)
   #:with  ats_stop <- (lookup-ξ* ξ_unstops nams_stop)
   #:with   ξ_stops := (extend-ξ*
                         ξ_unstops
                         (map (λ (n at) (cons n (TStop at)))
                              nams_stop
                              ats_stop))
   (InExpand
    (ζ (Stxξ ph (flip ph stx maybe-scp_i) ξ_stops) '∘ '• Σ*_0)
    `(,(SApp lbl `(,ph ,maybe-scp_i ,ξ) `(,(Sym 'local-expand2)) '())
      ,cont ,store ,Σ*_0))
   ev-lexpand]

  )

(define-unit-from-reduction ev-red@ -->)

(define-mixed-unit eval@
  (import (only config^
                AstEnv% Σ*%)
          (only terms-extra^
                val?)
          (only env^
                init-env)
          (only store^
                init-store)
          (only menv^
                init-ξ)
          (only mstore^
                init-Σ)
          (only expand^
                ==>))
  (export eval^)
  (inherit [ev-red@ reducer])
  (use-terms AstEnv Σ*)

  (define (--> delta) (λ () (reducer delta (==> delta))))

  ; eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*))
  (define (eval delta ph ast maybe-scp_i ξ Σ*)
    (define -->d (--> delta))
    (match-let ([(set `(,val ,done? ,_store ,Σ*_2) ...)
                 (apply-reduction-relation*
                  (-->d) `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                           • ,(init-store) ,Σ*))])
      (list->set
       (map (λ (vds) (cons (first vds) (third vds)))
            (filter (λ (vds) (and (val? (first vds)) (eq? (second vds) '•)))
                    (map list val done? Σ*_2))))))

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate delta ast)
    (for/set ([val+Σ* (in-set (eval delta 0 ast 'no-scope (init-ξ)
                                     (Σ* (init-Σ) (set) (set))))])
      (car val+Σ*))))






(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

(define (main [mode 'check])
  (run-examples run delta core:examples   mode α ≤a)
  (run-examples run delta phases:examples mode α ≤a)
  (run-examples run delta (append local:examples defs:examples)
                mode α ≤a))

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

