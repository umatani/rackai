#lang racket
(require
 "../set.rkt"
 "../reduction.rkt"
 "../mix.rkt"
 (only-in "../term.rkt" use-terms)
 "../example/suites.rkt"

 (only-in "../signatures.rkt"
          terms-extra^ syntax^ env^ store^ cont^ domain^ eval^
          menv^ mstore^ mcont^ bind^ parser^ expand^ expander^ run^ debug^)
 (only-in "../interp-base/core/config.rkt"
          config^ #%term-forms)

 (only-in "../terms.rkt"
          Var% Fun% App% If% VFun% List% Null% Pair% Atom% Bool% Sym%
          Stx% Prim% Hole%
          Lst id? lst->list snoc prim?)
 (only-in "../units.rkt"                  terms-extra@ io@)
 (only-in "../interp-base/units.rkt"      cont@ mcont@)
 (only-in "../interp-base/core/units.rkt" config@ debug@
          [syntax@ super:syntax@] expander@)
 (only-in "../interp-set/units.rkt"       domain@ env@ menv@ run@)
 (only-in "../interp-set/core/units.rkt" [eval@ set:eval@] parser@ expand/red@)
 (only-in "../interp-set/core/expander.rkt" [==> set:==>])
 (only-in "alloc.rkt" store@ mstore@ syntax::fin-alloc@ bind@))
(provide syntax@ eval@ run delta α ≤a)

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit [super:syntax@ empty-ctx zip unzip in-hole in-hole-stl
                          addremove strip subtract union add add-stl
                          at-phase update-ctx prune
                          flip flip-stl]
           [syntax::fin-alloc@ alloc-scope biggest-subset binding-lookup]))


;; Revise evaluate to filter out stuck states

(define-mixed-unit eval@
  (import (only config^
                AstEnv%)
          (only terms-extra^
                val?)
          (only env^
                init-env)
          (only store^
                init-store))
  (export eval^)
  (inherit [set:eval@ -->])
  (use-terms AstEnv)

  ; evaluate : Ast -> (Setof Val)
  (define (evaluate delta ast)
    (define -->d (--> delta))
    (match-let ([(set `(,val ,done? ,_store) ...)
                 (apply-reduction-relation*
                  -->d `(,(AstEnv ast (init-env)) • ,(init-store)))])
      (list->set
       (map car
            (filter (λ (vd) (and (val? (car vd)) (eq? (cdr vd) '•)))
                    (map cons val done?)))))))


;; Revise reduction rule ==>

;; ==> : ζ -> (Setof ζ)
(define-reduction (==> -->) #:super (set:==> -->)
  #:within-signatures [(only config^
                             AstEnv% TVar% ζ% Stxξ% κ% InEval%)
                       (only terms-extra^
                             val? stx? proper-stl?)
                       (only syntax^
                             empty-ctx zip unzip alloc-scope add flip in-hole)
                       (only env^
                             init-env)
                       (only store^
                             init-store)
                       (only menv^
                             init-ξ lookup-ξ extend-ξ)
                       (only mstore^
                             lookup-Σ alloc-name)
                       (only bind^
                             bind resolve id=?)
                       (only mcont^
                             push-κ)
                       (only parser^
                             parse)]

  ;; reference
  [(ζ (Stxξ (and id (Stx (Sym nam) ctx)) ξ) '∘ κ Σ)
   #:with nam <- (resolve id Σ)
   #:with  at <- (lookup-ξ ξ nam)
   ;(printf "ref: ~a ~a\n" nam at)
   (match at
     [(TVar id_new) (ζ id_new '• κ Σ)]
     [_ (error '==> "unbound identifier: ~a" nam)])
   ex-var])

(define-unit-from-reduction ex:red@ ==>)

(define-compound-unit/infer expand@
  (import terms-extra^ syntax^ config^ env^ store^ eval^ menv^ mstore^
          mcont^ bind^ parser^)
  (export expand^)
  (link expand/red@ ex:red@))


;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export run^ debug^)
   (link config@ terms-extra@ syntax@ env@ store@ cont@ eval@
         menv@ mstore@ bind@ mcont@ parser@ expand@ expander@ io@ run@ debug@))
  (import) (export run^ debug^))

(define-values/invoke-unit domain@
  (import) (export domain^))

;; run example
(define (main [mode 'check])
  (run-suite run delta (suite 'core) mode α ≤a))


;; alloc-nameのduplicate(衝突)への対策
;; にしても，{1, 2} ではなく，{2}だけなのは何故？
;; --> ξ, env の両方を set-based にする．
;; --> これで{1, 2}の両方が入る．
;; TODO: さらに，namじゃなくId(Stx)をeq?なkeyにして精度(uniquness)が上がり
;; concと同じ {1} だけに戻るはず．

(module+ test1
  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) (x))) 2)) 'eval)

  (run delta '(let ([z 1])
                ((let-syntax ([x (lambda (stx) #'z)])
                   (lambda (z) z)) 2)) 'eval))

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
  (run delta '((lambda (f x) (f x))
               (lambda (x) x)
               100) 'eval))

;; resolveに関連して問題が発生している <- 上のTODOのようなalloc-scopeの解決
;; で十分？ctx部分の表現の工夫(PRO時点)はどう影響？
