#lang racket

(define-signature red^ (reducer))

(begin-for-syntax
 (struct reduction-desc
   (unit-id params suepr-desc super-args
            within-signatures clause-map) #:transparent))

;;;;;;;; 1 ;;;;;;;;;;

;; (define-reduction (-->/+ <+>)
;;   [(cons a b) (<+> a b) 'add])
(define-syntax -->/+ (reduction-desc
                      #'-->/+@ #'(<+>)
                      #f #'() #'() '[(cons a b) (<+> a b)]))
(define-unit -->/+@ (import) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ (<+>) (λ (s)
                               (match s
                                 [(cons a b) (<+> a b)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer1 (reducer-of -->/+))
(define reducer1 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (() -->/+@    ;; (syntax-local-value #'-->/+)
                             )))))
((reducer1 +) (cons 3 4))


;;;;;;;; 2 ;;;;;;;;;;

(define-signature X^ (X))

;; (define-reduction (--->/+ <+>) #:within-signatures [X^]
;;   [(cons a b) (<+> a b X) 'add])
(define-syntax --->/+ (reduction-desc
                       #'--->/+@ #'(<+>)
                       #f #'() #'(X^) '[(cons a b) (<+> a b X)]))
(define-unit --->/+@ (import X^) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer )
       (λ (stx) #'(λ (<+>) (λ (s)
                               (match s
                                 [(cons a b) (<+> a b X)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

(define-unit X@ (import) (export X^)
  (define X 100))

;; (define reducer2 (reducer-of --->/+ #:within-units [X@]))
(define reducer2 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() --->/+@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer2 +) (cons 3 4))


;;;;;;;; 3 ;;;;;;;;;;

;; (define-reduction ==> #:super (-->/+ *))
(define-syntax ==> (reduction-desc
                    #'==>@ #'()
                    #'-->/+ #'(*) #'() '[]))
(define-unit ==>@ (import) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ () (λ (s)
                            (match s  ; (syntax-local-value #'-->/+)
                              [(cons a b) (* a b)]) 
                            ))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer3 (reducer-of ==>))
(define reducer3 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (() ==>@    ;; (syntax-local-value #'==>)
                             )))))
((reducer3) (cons 3 4))


;;;;;;;; 4 ;;;;;;;;;;

;; (define-reduction ===> #:super (--->/+ *) #:within-signatures [X^])
(define-syntax ===> (reduction-desc
                       #'===>@ #'()
                       #'--->/+ #'(*) #'(X^) '[]))
(define-unit ===>@ (import X^) (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ () (λ (s)
                            (match s ; (syntax-local-value #'--->/+)
                              [(cons a b) (* a b X)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer4 (reducer-of ===> #:within-units [X@]))
(define reducer4 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() ===>@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer4) (cons 3 4))
