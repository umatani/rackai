#lang racket/unit
(require racket
         "../struct-sig.rkt"
         "../syntax-sig.rkt"
         "../parse-sig.rkt")

(import (only struct^
              Stx atom? id? var fun app iif proper-stl?)
        (rename (only syntax^
                      unzip strip resolve id=?)
                [stx:id=? id=?]))
(export parse^)

;; ----------------------------------------
;; Simple parsing of already-expanded code
;;  (used for expand-time expressions, instead of
;;   modeling multiple phases):

;(: parse : Stx Σ -> Ast)
(define (parse stx Σ)
  (define (id=? nam) (λ (id) (stx:id=? id nam Σ)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'lambda)))
            ,(Stx stl_ids _) ,stx_body) _)
     (fun (map (λ (id) (var (resolve id Σ)))
               stl_ids)
          (parse stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'let)))
            ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (app (fun (map (λ (id) (var (resolve id Σ)))
                      stl_ids)
                 (parse stx_body Σ))
            (map (λ (stx_rhs) (parse stx_rhs Σ))
                 stl_rhs)))]
    ; (quote stx)
    [(Stx `(,(? id? (? (id=? 'quote))) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(Stx `(,(? id? (? (id=? 'syntax))) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(Stx (cons (? id? (? (id=? '#%app)))
                (Stx (cons stx_fun stl_args) _)) _)
     (app (parse stx_fun Σ)
          (parse* stl_args Σ))]
    ; (if stx stx stx)
    [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (iif (parse stx_test Σ)
          (parse stx_then Σ)
          (parse stx_else Σ))]
    ; reference
    [(? id? id) (var (resolve id Σ))]
    ; literal
    [(Stx (? atom? atom) _) atom]))

;(: parse* : Stl Σ -> (Listof Ast))
(define (parse* stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse stx Σ) (parse* stl Σ))]
    [stx (list (parse stx Σ))]))

(define parser parse)

