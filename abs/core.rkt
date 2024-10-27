#lang racket/base
(require
 racket/unit
 (only-in racket/list        remove-duplicates append-map)
 (only-in racket/match       match match-let)
 "../interpreter.rkt"
 "../signatures.rkt"
 (only-in "../reduction.rkt" define-reduction define-unit-from-reduction
                             apply-reduction* enable-tracing)
 (only-in "../nondet.rkt"    do := <- pure lift results)
 (only-in "../set.rkt"       set set? ∅ ∅? set→list list→set set-map)
 (only-in "../mix.rkt"       define-mixed-unit inherit)
 (only-in "../syntax.rkt"    snoc)
 "../test/suites.rkt"
 "../base/core/terms.rkt"
 (only-in "../mult/core/units.rkt"
          io@ cont@ mcont@ debug@ expand@ expander@ syntax@ domain@
          env@ menv@ run@ eval@ parse@ parser@ [bind@ mult:bind@] id@)
 (only-in "../mult/core/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "alloc.rkt"               store@ mstore@
                                    biggest-subset binding-lookup))
(provide syntax@ evaluator@ main-minus@
         interp eval-->* expand==>*)


;;;; bind^

(define-mixed-unit bind@
  (import  (only mstore^    lookup-Σ))
  (export  bind^)
  (inherit [mult:bind@      bind])

  ; resolve : Id Σ -> (SetM Nam)
  (define (resolve id Σ0)
    (match-let ([(Stx (Sym nam) ctx) id])
      ;(printf "resolve: ~a\n" nam)
      (let* ([sbss (filter set? (set→list (results (lookup-Σ Σ0 nam))))]
             ;[_ (printf "sbss: ~a\n" sbss)]
             [scpsss
              (let ([scpsss (map (λ (sbs)
                                   (set-map (λ (sb) (StoBind-scps sb)) sbs))
                                 sbss)])
                (map remove-duplicates scpsss))]
             ;[_ (printf "scpsss: ~a\n" scpsss)]
             [scps_biggests (remove-duplicates
                             (append-map (λ (scpss)
                                           (biggest-subset ctx scpss))
                                         scpsss))]
             ;[_ (printf "scps_biggests: ~a\n" scps_biggests)]
             [nam_biggests
              (remove-duplicates
               (apply append
                      (for*/list ([sbs (in-list sbss)]
                                  [scps_biggest (in-list scps_biggests)])
                        (binding-lookup sbs scps_biggest))))])
        ;(printf "nam_biggests: ~a\n" nam_biggests)
        (let ([r (if (null? nam_biggests)
                   (set nam)
                   (list→set nam_biggests))])
          ;(printf "resolve done: ~a\n" r)
          (lift r))))))


;;;; Evaluator
;;;;   filter out stuck states

(define-unit evaluator@
  (import
   (only   env^    init-env)
   (only store^    init-store)
   (only  eval^    -->))
  (export evaluator^)

  ;; evaluator : Ast → (SetM Val)
  (define (evaluator δ ast)
    (define -->d (--> δ))

    (do `(,val ,done? ,_store) <- (apply-reduction*
                                   -->d `(,(AstEnv ast (init-env))
                                          ● ,(init-store)))
        (if (and (val? val) (eq? done? '●))
          (pure val)
          (lift ∅)))))


;;;; Main

(define-compound-unit/infer main-minus@
  (import domain^ eval^ expand^ parser^)
  (export syntax^ env^ store^ cont^ evaluator^ menv^ mstore^ bind^ id^ mcont^
          run^ debug^)
  (link   syntax@ env@ store@ cont@  evaluator@ menv@ mstore@ bind@ id@ mcont@
          expander@ io@ run@ debug@))

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^ debug^)
   (link main-minus@
         domain@ eval@ parse@ parser@ expand@))
  (import) (export domain^ run^ debug^))

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
