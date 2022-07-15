#lang racket
(require racket/struct
         (for-syntax racket racket/syntax
                     syntax/parse syntax/stx syntax/id-table))
(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class maybe-id
    (pattern name:id)
    (pattern #f))
  (define-syntax-class field
    (pattern name:id)
    (pattern [name:id default:expr]))

  (define (cls-of id . ids)
    (if (null? ids)
        (format-id id "~a%" id)
        (map (λ (id) (format-id id "~a%" id)) (cons id ids)))))

(define-syntax (define-term stx)
  (syntax-parse stx
    [(_ name:id (f ...))
     #'(define-term name #f (f ...))]
    [(_ name:id super:maybe-id (f:field ...))
     #'(define-term name super (f ...) #:remove [])]
    [(_ name:id super:maybe-id (f:field ...) #:remove [r:id ...])
     
     #:with super-cls (if (syntax->datum #'super)
                          (cls-of #'super)
                          #'object%)
     #`(define #,(cls-of #'name)
         (class* super-cls (#;writable<%>)
           (inspect #f)
           (init-field f ...)
           (super-new [r #f] ...)
           #;
           (define/public (custom-write port)
             (write `(name ,f ...) port))
           #;
           (define/public (custom-display port)
             (custom-write port))))]))

(define-syntax (define-term-form stx)
  (syntax-parse stx
    #:datum-literals [constructor matcher]
    [(_ term:id (name:id fld:id ...))
     #'(define-term-form term
         [constructor (name fld ...)]
         [matcher     (name fld ...)])]
    [(_ term:id
        [constructor (cname:id cfld:id ...)]
        [matcher     (mname:id mfld:id ...)])
     #:with pred-id (format-id #'mname "~a?" #'cname)
     #:with (accessor-id ...) (stx-map
                               (λ (f)
                                 (format-id #'mname "~a-~a" #'mname f))
                               #'(mfld ...))
     #:with (mparam ...) (generate-temporaries #'(mfld ...))
     #:with (cparam ...) (generate-temporaries #'(cfld ...))
     #`(begin
         (define pred-id (is-a?/c term))
         (define (accessor-id x) (get-field mfld x))
         ...
         #,@(if (free-identifier=? #'cname #'mname)
                #'((define-match-expander cname
                     (λ (stx) (syntax-case stx ()
                                 [(_ mparam ...)
                                  #'(? pred-id
                                      (app accessor-id mparam) ...)]))
                     (λ (stx) (syntax-case stx ()
                                 [(_ cparam ...)
                                  #'(new term [cfld cparam] ...)]))))
                #'((define-match-expander mname
                     (λ (stx) (syntax-case stx ()
                                 [(_ mparam ...)
                                  #'(? pred-id
                                      (app accessor-id mparam) ...)])))
                   (define (cname cparam ...)
                     (new term [cfld cparam] ...)))))]))

(define-syntax (use-term stx)
  (syntax-parse stx
    [(_ name:id)
     #:do [(define term-forms
             (syntax-local-value (datum->syntax
                                  #'name #;stx
                                  '#%term-forms)))]
     #:with rhs #`(name #,@(cdr (assoc (syntax->datum #'name) term-forms)))
     #`(define-term-form #,(cls-of #'name) rhs)]
    [(_ (name:id fld:id ...))
     #`(define-term-form #,(cls-of #'name) (name fld ...))]
    [(_ [term:id (~and ctr (:id ...+)) (~and mtr (:id ...+))])
     #'(define-term-form term [constructor ctr] [matcher mtr])]))

(define-syntax (use-terms stx)
  (syntax-parse stx
    [(_ spec ...)
     #'(begin (use-term spec) ...)]))
