#lang racket

(define-signature red^ (reducer))

(begin-for-syntax
 (struct reduction-desc
   (unit-id
    maybe-sig-id ;; #:doが書かれている場合だけdefineされている変数をexport
    params
    suepr-desc
    super-args
    within-signatures clause-map) #:transparent))

;;;;;;;; 1 ;;;;;;;;;;

;; (define-reduction (-->/+ <+>)
;;   [(cons a b) (<+> a b) 'add])
(define-syntax -->/+ (reduction-desc
                      #'-->/+@ #f
                      #'(<+>)
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
                       #'--->/+@ #f
                       #'(<+>)
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
                    #'==>@ #f
                    #'()
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
                     #'===>@ #f
                     #'()
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



;;;;;;;; 5 ;;;;;;;;;;
;;;; #:do [...]

;; (define-reduction (~~>/+ <+>) #:within-signatures [X^]
;;   #:do [(define Y 300)
;;         (println 'HOGEEEE-IN-DO)
;;         (define-syntax (m stx) #'(dbgX 'M))
;;         (define (dbgX msg) (println msg) (+ Y X))]
;;   [(cons a b) (<+> a b (m) (dbgX 'HOGEE) Y) 'add])
(define-syntax ~~>/+ (reduction-desc
                      #'~~>/+@ #'~~>/+^
                      #'(<+>)
                      #f #'() #'(X^)
                      '[(cons a b) (<+> a b (m) (dbgX 'HOGEE) Y)]))
(define-signature ~~>/+^
  (Y
   dbgX
   (define-syntaxes (m) (λ (stx) #'(dbgX 'M)))
   ))
(define-unit ~~>/+@ (import X^) (export red^ ~~>/+^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ (<+>)
                     (λ (s)
                       (match s
                         [(cons a b) (<+> a b (m) (dbgX 'HOGEE) Y)])))))))
  (define-unit M@ (import) (export M^))

  (define Y 300)
  (println 'HOGEEEE-IN-DO)
  (define (dbgX msg) (println msg) X)
  (define-syntaxes (m) (λ (stx) #'(dbgX 'M)))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

;; (define reducer5 (reducer-of ~~>/+ #:within-units [X@]))
(define reducer5 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (() ~~>/+@    ;; (syntax-local-value #'--->/+)
                             x)))))
((reducer5 +) (cons 3 4))

;;;;;;;; 6 ;;;;;;;;;;
;;;; #:do [...] を含む reducton を，その定義を含めて継承

;; (define-reduction ~~~> #:super (~~>/+ *) #:within-signatures [X^])
(define-syntax ~~~> (reduction-desc
                     #'~~~>@ #f
                     #'()
                     #'~~>/+ #'(*) #'(X^) '[]))

(define-unit ~~~>@
  (import X^ ~~>/+^) ;; 先祖のmaybe-sig-id集合
  (export red^)
  (define-signature M^
    ((define-values (#%-->) (#%reducer))
     (define-syntaxes (#%reducer)
       (λ (stx) #'(λ ()
                     (λ (s)
                       (match s
                         [(cons a b) (* a b (m) (dbgX 'HOGEE) Y)])))))))
  (define-unit M@ (import) (export M^))
  (define reducer (invoke-unit
                   (compound-unit
                    (import) (export)
                    (link (([m : M^]) M@)
                          (() (unit (import M^) (export)
                                #%-->) m)))))
  reducer)

; 余計なユニットがcompoundに含まれてても良いかの確認 --> OK
(define-signature Y^ (Y))
(define-unit Y@ (import) (export Y^)
  (define Y 12345))

;; (define reducer6 (reducer-of ~~~> #:within-units [X@]))
(define reducer6 (invoke-unit
                  (compound-unit
                   (import) (export)
                   (link (([x : X^]) X@)
                         (([y : Y^]) Y@)
                         ;; maybe-sig-idが#fでない先祖毎に以下の行．
                         ;; importしてないものも含め全てimport側に渡して大丈夫
                         (([s : ~~>/+^]) ~~>/+@ x y) 
                         (() ~~~>@    ;; (syntax-local-value #'~~~>)
                             s x y)))))
((reducer6) (cons 3 4))
