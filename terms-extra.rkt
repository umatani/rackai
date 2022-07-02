#lang racket
(require
 (only-in "term.rkt"  use-terms)
 (only-in "terms.rkt" terms^ #%term-forms))
(provide terms-extra^ terms-extra@)

(define-signature terms-extra^
  (id stx? stl? proper-stl? id? atom? prim?
   stx-prim? val? nam? state? tm? cont? ser?))

(define-unit terms-extra@
  (import (only terms^
                Sym% Stx% Stxξ% Hole% 𝓁% Defs% VFun% LBind2% Store%
                KApp% KIf% AstEnv% SApp% SIf% SSeq%))
  (export terms-extra^)

  (use-terms Sym Stx Stxξ Hole 𝓁 Defs VFun LBind2 Store KApp KIf
             AstEnv SApp SIf SSeq)

  (define (id nam ctx) (Stx (Sym nam) ctx))

  ;; Additional predicates
  (define (stx? x)
    (or (and (Stx? x) (atom? (Stx-e x)))
        (and (Stx? x) (pair? (Stx-e x))
             (stx? (car (Stx-e x)))
             (stl? (cdr (Stx-e x))))
        (and (Stx? x) (proper-stl? (Stx-e x)))
        (Stxξ? x)
        (Hole? x)
        (and (Stx? x) (Hole? (Stx-e x)))))

  (define (stl? x)
    (or (null? x)
        (stx? x)
        (and (pair? x) (stx? (car x)) (stl? (cdr x)))
        (Hole? x)))

  (define (proper-stl? x)
    (or (null? x) (and (pair? x) (stx? (car x)) (proper-stl? (cdr x)))))

  (define (id? x) (and (Stx? x) (Sym? (Stx-e x))))

  (define (atom? x)
    (or (null? x) (boolean? x) (real? x) (Sym? x) (prim? x)
        (𝓁? x) (Defs? x) ;; used only in full
        ))

  (define (prim? x)
    (or (member x '(syntax-e datum->syntax + - * / < = eq?
                             cons car cdr list second third fourth
                             printe ;; for debug
                             ))
        (stx-prim? x)))

  (define (stx-prim? x)
    (member x '(syntax-local-value local-expand
                                   syntax-local-identifier-as-binding
                                   box unbox set-box!
                                   syntax-local-make-definition-context
                                   syntax-local-bind-syntaxes)))

  (define (val? x)
    (or (VFun? x)
        (atom? x)
        (and (pair? x) (val? (car x)) (val? (cdr x)))
        (stx? x)
        (LBind2? x)))

  (define (nam? x) (symbol? x))

  (define (state? x)
    (and (list? x)
         (= (length x) 3)
         (tm? (first x))
         (cont? (second x))
         (Store? (third x))))

  (define (tm? x) (or (val? x) (ser? x)))

  (define (cont? x) (or (eq? x '•) (KApp? x) (KIf? x)))

  (define (ser? x) (or (AstEnv? x) (SApp? x) (SIf? x) (SSeq? x))))
