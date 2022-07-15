#lang racket
(require (for-syntax racket racket/syntax racket/unit-exptime
                     syntax/parse syntax/stx syntax/id-set))
(provide define-mixed-unit)

;; Simple unit wrapper for mixins

(define-syntax (define-mixed-unit stx)
  (define-syntax-class member-spec
    #:attributes [from to]
    (pattern member:id
             #:with from #'member
             #:with to   #'member)
    (pattern [to:id from:id]))

  (define-syntax-class inherit-spec
    (pattern [uid:id m:member-spec ...]
             #:with (from ...) #'(m.from ...)
             #:with (to ...)   #'(m.to ...)))

  (define-syntax-class import-spec
    #:attributes [name]
    #:literals [only]
    (pattern name:id)
    (pattern (only name:id x:id ...)))


  (define (signatures-of unit-id)
    (call-with-values
     (λ () (unit-static-signatures unit-id unit-id))
     (λ (is es)
       (unless (= (length es) 1)
         (raise-syntax-error 'define-mixed-unit
                             "multiple exports not supperted"
                             (map (compose1 syntax->datum cdr) es)))
       (list (map cdr is) (cdar es)))))

  (define (attach-tag/prefix exports)
    (for/fold ([seen (set)]
               [t&p '()]
               #:result (reverse t&p))
              ([e (in-list (syntax->list exports))])
      (let ([sym (syntax->datum e)])
        (if (set-member? seen sym)
            (values
             seen
             (cons #`(tag #,(generate-temporary 't)
                          (prefix #,(format-id e "~a:" (gensym sym))
                                  #,e)) t&p))
            (values
             (set-add seen sym)
             (cons #`(prefix #,(format-id e "~a:" (gensym sym))
                             #,e) t&p))))))

  (define (prefix-id-of tag&prefix)
    (syntax-parse tag&prefix
      #:literals [tag prefix]
      [(tag t:id (prefix pre:id e)) #'pre]
      [(prefix pre:id e) #'pre]))

  (define (attach-tag-to-link prefixes links)
    (stx-map (λ (prefix link)
               (syntax-parse prefix
                 #:literals [tag prefix]
                 [(tag t:id (prefix pre:id e)) #`(tag t #,link)]
                 [(prefix pre:id e) link]))
             prefixes links))

  (syntax-parse stx
    #:literals [import export inherit]
    [(_ name:id (import i:import-spec ...) (export e ...)
        (inherit ih:inherit-spec ...)
        body ...)
     #:with tmp-unit (datum->syntax #'name
                                    (gensym
                                     (syntax->datum #'name)))
     #:with (uid ...) #'(ih.uid ...)
     #:with (el ...) (generate-temporaries #'(e ...))

     #:with (((ii ...) ie) ...) (stx-map signatures-of #'(uid ...))
     #:do [(define imports (list->mutable-seteq
                            (syntax->datum #'(i.name ... ii ... ...))))]
     #:with (ii* ...) (map (λ (s) (datum->syntax #'name s))
                           (set->list imports))

     #:with (pre ...) (attach-tag/prefix #'(ie ...))
     
     #:with ((from-m ...) ...) (map
                                (λ (p ms)
                                  (stx-map
                                   (λ (m) (format-id p "~a~a" p m))
                                   ms))
                                (stx-map prefix-id-of #'(pre ...))
                                (syntax->list #'((ih.from ...) ...)))
     
     #:with (il ...)  (generate-temporaries #'(uid ...))
     #:with (il* ...) (attach-tag-to-link #'(pre ...) #'(il ...))

     (let ([rstx #'(begin
                     (define-unit tmp-unit
                       (import i ... pre ...)
                       (export e ...)
                       (define ih.to from-m) ... ...
                       body ...)
                     (define-compound-unit/infer name
                       (import ii* ...)
                       (export el ...)
                       (link (([il : ie]) uid el ...) ...
                             (([el : e] ...) tmp-unit il* ...))))])
       ;(pretty-print (syntax->datum rstx))
       rstx)]))
