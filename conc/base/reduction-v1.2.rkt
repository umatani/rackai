#lang racket
(require racket/provide-syntax racket/require-syntax
         "set.rkt" "queue.rkt" "nondet.rkt"
         (for-syntax racket racket/syntax
                     syntax/parse syntax/stx
                     racket/unit-exptime))
(provide (all-defined-out)
         (all-from-out "nondet.rkt")
         (for-syntax reduction-sig-id reduction-within-signatures
                     signature-variables prefix-id))

;;;; non-deterministic reduction engine

(define-signature red^
  (reducer ;; Param ... -> State -> (Setof State)
   ))

(begin-for-syntax
  (define (make-clause-map clauses)
    (define names (map (compose1 last syntax->datum) clauses))
    (when (check-duplicates names eq?)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/fold ([map    (hasheq)])
              ([name   (in-list names)]
               [clause (in-list clauses)])
      (hash-set map name clause)))

  (define (make-match-body bs)
    (syntax-parse bs
      [(b) #`((pure b))]
      [(#:when t b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(#:failif (not t) #f b2 ...))]
      [(#:with x assign-id:assign e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(x assign-id e b2 ...))]
      [(#:with x elem-id:elem e b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #`(x elem-id e b2 ...))]
      [(b1 b ...)
       (with-syntax ([(b2 ...) (make-match-body #'(b ...))])
         #'(b1 b2 ...))]))

  (struct reduction (sig-id within-signatures) #:transparent)

  (struct reduction-desc (params
                          super/meta super-args clause-map) #:transparent)

  (define (make-reducer-body red red-desc s maybe-args sub-clause-names)
    (define (stx-rescope stx)
      (datum->syntax red (if (syntax? stx) (syntax->datum stx) stx)))

    (define body (let ([super/meta (reduction-desc-super/meta red-desc)]
                       [super-args (reduction-desc-super-args red-desc)]
                       [clause-map (reduction-desc-clause-map red-desc)])
                   (if (syntax->datum super/meta) ;; not #f
                       (make-reducer-body
                        red ;; super??
                        (syntax-local-value super/meta)
                        s
                        super-args
                        (append sub-clause-names (hash-keys clause-map)))
                       #'(set))))
    (let* ([args (or maybe-args #'())]
           [params (if maybe-args
                       (reduction-desc-params red-desc)
                       #'())]
           [clause-map (reduction-desc-clause-map red-desc)])
      (unless (= (length (syntax->list params))
                 (length (syntax->list args)))
        (raise-syntax-error
         'define-parameterized-extended-reduction-relation
         (format "red args arity mismatch: ~a ~a"
                 (syntax->datum params)
                 (syntax->datum args))))
      (with-syntax ([(param ...) (stx-rescope params)]
                    [(arg ...)   (stx-rescope args)])
        #`(let-syntax ([param (make-rename-transformer #'arg)]
                       ...)
            #,(for/fold ([body body])
                        ([clause
                          (for/list
                              ([(k v) (in-hash clause-map)]
                               #:when (not (member k sub-clause-names)))
                            v)])
                (syntax-case (stx-rescope clause) ()
                  [(p b ... rule-name)
                   #`(let ([nexts #,body])
                       (match #,s
                         [p (set-union
                             nexts
                             (car (do #,@(make-match-body #'(b ...)))))]
                         [_ nexts]))]))))))

  (define-syntax-class red-spec
    (pattern name:id
             #:with params #'())
    (pattern (name:id param:id ...)
             #:with params #'(param ...)))

  (define-splicing-syntax-class options-spec
    (pattern (~seq (~alt (~optional (~seq #:super s:red-spec)
                                    #:name "#:super option")
                         (~optional (~seq #:within-signatures [sig-spec ...])
                                    #:name "#:within-units option"))
                   ...)
             #:with name #'(~? s.name #f)
             #:with args #'(~? s.params ())
             #:with sigs #'(~? (sig-spec ...) ())))
  )


(define-syntax (define-reduction stx)
  (syntax-parse stx
    [(_ r:red-spec opts:options-spec
        (~and [pat body ...+ name] clause) ...)
     #:with red-st #'r.name
     #:with red-sig (format-id #'red-st "~a^" #'red-st)
     #:with red/meta (format-id #'red-st "~a/meta" #'red-st)
     #:with super-red-st #'opts.name
     #:with super-red-sig (and (syntax->datum #'super-red-st)
                               (format-id #'super-red-st
                                          "~a^" #'super-red-st))
     #:with super-red/meta (and (syntax->datum #'super-red-st)
                                (format-id #'super-red-st
                                           "~a/meta" #'super-red-st))
     #:with (param ...) #'r.params
     #:with (arg ...)   #'opts.args
     #:with (within-sig-id ...) #'opts.sigs
     (let* ([clause-map (make-clause-map (syntax->list #'(clause ...)))]
            [reduction-str (reduction #'red-sig #'(within-sig-id ...))])
       #`(begin
           (define-signature red-sig
             #,@(if (syntax->datum #'super-red-sig)
                    #`(extends super-red-sig)
                    #'())

             ((open within-sig-id)
              ...
              (define-values (red-sig) (#%reducer))
              (define-syntaxes (red/meta #%reducer)
                (values
                 (reduction-desc #'(param ...)
                                 #'super-red/meta
                                 #'(arg ...) #,clause-map)
                 (λ (stx) #`(λ (param ...)
                               (λ (s) #,(make-reducer-body
                                          #'red-st
                                          (syntax-local-value #'red/meta)
                                          #'s #f '()))))))))
           (define-syntax red-st #,reduction-str)))]))

(define-provide-syntax (reduction-out stx)
  (syntax-case stx ()
    [(_ red ...)
     #`(combine-out red ...
                    #,@(stx-map (λ (red) (format-id red "~a^" red))
                                #'(red ...)))]))

(define-require-syntax (reduction-in stx)
  (syntax-case stx ()
    [(_ path red ...)
     #`(combine-in (only-in path red ...)
                   (only-in path #,@(stx-map (λ (red)
                                               (format-id red "~a^" red))
                                             #'(red ...))))]))

(begin-for-syntax
  (define (pair->tagged-sig-id p)
    (if (car p)
        #`(tag #,(datum->syntax (cdr p) (car p)) #,(cdr p))
        (cdr p)))

  (define ((prefix-id p) id)
    (format-id id "~a~a" p id))

  (define (unit-static-imports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (impts _b) (map pair->tagged-sig-id impts))))

  (define (unit-static-exports unit-id err-syntax)
    (call-with-values (λ () (unit-static-signatures unit-id err-syntax))
                      (λ (_a expts) (map pair->tagged-sig-id expts))))

  (define (signature-variables sig-id err-syntax)
    (call-with-values (λ () (signature-members sig-id err-syntax))
                      (λ (_a vars _b _c) vars))))


(define-syntax (reduction->unit stx)
  (syntax-parse stx
    [(_ red)
     #:do [(define red-st (syntax-local-value #'red))]
     #:with red-sig (reduction-sig-id red-st)
     #:with (within-sig-id ...) (syntax-local-introduce
                                 (reduction-within-signatures red-st))
     #:with (link-id ...) (stx-map (λ (sig-id)
                                     (generate-temporary sig-id))
                                   #'(within-sig-id ...))
     #:with (pre ...) (stx-map (λ (sig-id)
                                 (generate-temporary sig-id))
                               #'(within-sig-id ...))
     #:with ((var ...) ...) (stx-map (λ (sig-id)
                                       (signature-variables sig-id stx))
                                     #'(within-sig-id ...))
     #:with ((pvar ...) ...) (stx-map (λ (pre vars)
                                        (stx-map (prefix-id pre) vars))
                                      #'(pre ...) #'((var ...) ...))
     #:with ((var2 ...) ...) (stx-map (λ (vars)
                                        (stx-map syntax-local-introduce vars))
                                      #'((var ...) ...))
     #'(compound-unit
        (import [link-id : within-sig-id] ...) (export r)
        (link (([s : red-sig]) (unit
                                 (import (prefix pre within-sig-id) ...)
                                 (export red-sig)
                                 (define var2 pvar) ... ...)
                               link-id ...)
              (([r : red^]) (unit (import red-sig) (export red^)
                              (define reducer red-sig) reducer) s)))]))

;; reductionとは直接関係のない unit の便利ユーティリティ
(define-syntax (compose-unit stx)
  (syntax-parse stx
    [(_ unit-id ...)
     #:with ((i-id ...) ...) (stx-map (λ (uid)
                                        (unit-static-imports uid stx))
                                      #'(unit-id ...))
     #:with ((i-link ...) ...) (stx-map (λ (i-ids)
                                          (stx-map generate-temporary i-ids))
                                        #'((i-id ...) ...))
     #:with ((e-id ...) ...) (stx-map (λ (uid)
                                        (unit-static-exports uid stx))
                                      #'(unit-id ...))
     #:with ((e-link ...) ...) (stx-map (λ (e-ids)
                                          (stx-map generate-temporary e-ids))
                                        #'((e-id ...) ...))
     #'(compound-unit
        (import [i-link : i-id] ... ...) (export e-link ... ...)
        (link (([e-link : e-id] ...) unit-id i-link ...)
              ...))]))


;; (reducer-of red) : State -> (Setof State)
(define-syntax (reducer-of stx)
  (syntax-parse stx
    [(_ red)
     #'(reducer-of red #:within-units [])]
    [(_ red #:within-units [unit-id ...])
     #:with (within-sig-id ...) (reduction-within-signatures
                                 (syntax-local-value #'red))
     #:with (link-id ...) (stx-map (λ (sig-id)
                                     (generate-temporary sig-id))
                                   #'(within-sig-id ...))
     #:with ((i-id ...) ...) (stx-map (λ (uid) (unit-static-imports uid stx))
                                      #'(unit-id ...))
     #:with ((i-link ...) ...) (stx-map (λ (i-ids)
                                          (stx-map generate-temporary i-ids))
                                        #'((i-id ...) ...))
     #'(invoke-unit (compound-unit
                     (import [i-link : i-id] ... ...) (export)
                     (link (([link-id : within-sig-id] ...)
                            (compose-unit unit-id ...)
                            i-link ... ...)
                           (() (reduction->unit red) link-id ...)))
                    (import i-id ... ...))]))


;(: apply-reduction-relation* (∀ [A] (->* ((-> A (Setof A)) A)
;                                          (#:steps (Option Natural))
;                                          (Setof A))))
(define (apply-reduction-relation* --> s #:steps [steps #f])
  (let ([all-states (mutable-set)]
        [normal-forms (mutable-set)]
        [worklist (make-queue)])
    (define (loop steps)
      (unless (or (queue-empty? worklist)
                  (and steps (<= steps 0)))
        (let* ([s (dequeue! worklist)]
               [nexts (--> s)])
          (if (set-empty? nexts)
              (set-add! normal-forms s)
              (for ([next (in-set nexts)]
                #:when (not (set-member? all-states next)))
                (set-add! all-states next)
                (enqueue! worklist next))))
        (loop (and steps (sub1 steps)))))
    (enqueue! worklist s)
    (loop steps)
    (if steps all-states normal-forms)))
