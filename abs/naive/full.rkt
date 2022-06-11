#lang racket
(require "../../conc/base/set.rkt"
         "../../conc/base/reduction.rkt"
         (only-in "../../conc/base/example.rkt"
                  core:examples phases:examples local:examples defs:examples)
         (only-in "../../conc/base/core/misc.rkt" run-examples)

         ;; toriaezu
         (only-in "../../conc/base/core/delta.rkt" delta)

         (only-in "../../conc/base/core/syntax.rkt" zip unzip snoc union)
         (only-in "../../conc/base/core/eval.rkt"
                  init-env update-env lookup-env init-store)
         (only-in "../../conc/base/core/expand.rkt"
                  init-ξ extend-ξ lookup-ξ push-κ lookup-κ init-Θ init-Σ)
         (only-in "../../conc/base/phases/syntax.rkt"
                  empty-ctx add flip prune at-phase)
         (only-in "../../conc/base/phases/expand.rkt"
                  id-seq id-kont id-snoc stx-nil)
         (only-in "../../conc/base/phases/main.rkt" reader printer)
         (only-in "../../conc/base/full/syntax.rkt" in-hole)
         (only-in "../../conc/base/full/expand-eval.rkt"
                  extend-ξ* unstop)
         (only-in "../../conc/base/full/main.rkt" run-all/runs)

         ;; Set-based Heap version
         (only-in "../../conc/base+set-heap/core/misc.rkt" define-runner)
         (only-in "../../conc/base+set-heap/core/eval.rkt"
                  lookup-store update-store* alloc-loc* push-cont)
         (only-in "../../conc/base+set-heap/core/expand.rkt"
                  alloc-name alloc-scope)
         (only-in "../../conc/base+set-heap/phases/syntax.rkt" bind resolve)
         (only-in "../../conc/base+set-heap/phases/expand.rkt" regist-vars)
         (only-in "../../conc/base+set-heap/phases/parse.rkt" parse)
         (only-in "../../conc/base+set-heap/full/syntax.rkt"
                  resolve*/resolve id=?)
         (only-in "../../conc/base+set-heap/full/expand-eval.rkt"
                  alloc-box box-lookup box-update
                  alloc-def-ξ def-ξ-lookup def-ξ-update
                  [-->f/store base+set-heap:-->f/store]
                  [==>f/Σ base+set-heap:==>f/Σ])

         ;; Abstract version
         "struct-full.rkt"
         (only-in "delta.rkt" δ)
         (only-in "core.rkt"
                  [run core:run])
         (only-in "phases.rkt" [run phases:run])

         (for-syntax racket/list))
(provide (all-defined-out))

(define-parameterized-extended-reduction-relation (-->f/store delta ==>f)
  (base+set-heap:-->f/store delta ==>f))

(define-parameterized-extended-reduction-relation (==>f/Σ -->f)
  (base+set-heap:==>f/Σ -->f))

(define-values (-->f ==>f)
  (letrec ([-->f (λ () ((reducer-of -->f/store) #;δ delta ==>f))]
           [==>f (λ () ((reducer-of ==>f/Σ) -->f))])
    (values (-->f) (==>f))))

;; redefine drivers to refer to new struct-full.rkt


;(: eval : Ph Ast MaybeScp ξ Σ* -> (Setof (Cons Val Σ*)))
(define (eval ph ast maybe-scp_i ξ Σ*)
  (match-let ([(set `(,(? Val? val) • ,_store ,Σ*_2) ...)
               (apply-reduction-relation*
                -->f `(,(AstEnv ph ast (init-env) maybe-scp_i ξ)
                       • ,(init-store) ,Σ*))])
    (list->set (map cons val Σ*_2))))

;(: evaluate : Ast -> (Setof Val))
(define (evaluate ast)
  (for/set ([val+Σ*
             (in-set (eval 0 ast 'no-scope (init-ξ)
                           (Σ* (init-Σ) (set) (set))))])
    (car val+Σ*)))

;(: expand : Ph Stx ξ Σ* -> (Setof (Cons Stx Σ*)))
(define (expand ph stx ξ Σ*)
  (let ([init-ζ (ζ (Stxξ ph stx ξ) '∘ '• (init-Θ) Σ*)])
    (match-let ([(set (ζ stx_new '• '• Θ_new Σ*_new) ...)
                 (apply-reduction-relation* ==>f init-ζ)])
      (list->set (map cons stx_new Σ*_new)))))

(define (expander stx)
  (expand 0 stx (init-ξ) (Σ* (init-Σ) (set) (set))))

;(: parser : Stx Σ* -> Ast)
(define (parser stx Σ*) (parse 0 stx (Σ*-Σ Σ*)))


(define-runner run
  reader printer
  expander parser evaluate)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run (append local:examples defs:examples) mode))

(define run-all (run-all/runs core:run phases:run run))

;; for debug

;(: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

;(: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
