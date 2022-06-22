#lang racket/unit
(require
 racket/match

 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "../../../syntax-sig.rkt"        syntax^)
 (only-in "../../../mstore-sig.rkt"        mstore^)
 (only-in "../../../parse-sig.rkt"         parse^))

(import (only struct-common^
              Stx atom? id? var fun iif app proper-stl?)
        (only syntax^ strip unzip)
        (rename (only mstore^ resolve id=?) [msto:id=? id=?]))
(export parse^)


;; ----------------------------------------
;; Simple parsing of already-expanded code

;; This parse is the same as the single-phase one, but with `ph`
;; threaded through to `resolve`
; parse : Ph Stx Σ -> Ast
(define (parse ph stx Σ)
  (define (id=? nam) (λ (id) (msto:id=? ph id nam Σ)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'lambda)))
            ,(Stx stl_ids _) ,stx_body) _)
     (fun (map (λ (id) (var (resolve ph id Σ)))
               stl_ids)
          (parse ph stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(Stx `(,(? id? (? (id=? 'let)))
            ,(Stx (? proper-stl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (app (fun (map (λ (id) (var (resolve ph id Σ)))
                      stl_ids)
                 (parse ph stx_body Σ))
            (map (λ (stx_rhs) (parse ph stx_rhs Σ))
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
     (app (parse ph stx_fun Σ)
          (parse* ph stl_args Σ))]
    ; (if stx stx stx)
    [(Stx `(,(? id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (iif (parse ph stx_test Σ)
          (parse ph stx_then Σ)
          (parse ph stx_else Σ))]
    ; reference
    [(? id? id) (var (resolve ph id Σ))]
    ; literal
    [(Stx (? atom? atom) _) atom]))

; parse* : Ph Stl Σ -> (Listof Ast)
(define (parse* ph stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse ph stx Σ) (parse* ph stl Σ))]
    [stx (list (parse ph stx Σ))]))

; parser : Stx Σ -> Ast
(define (parser stx Σ) (parse 0 stx Σ))
