#lang racket
(require (for-syntax racket racket/syntax
                     syntax/parse syntax/stx))

(module experiment0 racket
  (require (for-syntax racket racket/syntax
                       syntax/parse syntax/stx syntax/strip-context))

  (define-for-syntax (make-clause-map clauses)
    (define names (map (compose1 last syntax->datum) clauses))
    (when (check-duplicates names eq?)
      (raise-syntax-error #f "duplicate reduction names" names))
    (for/list ([name   (in-list names)]
               [clause (in-list clauses)])
      (cons name clause)))
  (define-for-syntax (clause-map-rule-names clause-map)
    (map car clause-map))
  (define-for-syntax (clause-map-clauses clause-map)
    (map cdr clause-map))
  (define-for-syntax (get-clause-body clause-map rule-name)
    (syntax-parse (cdr (assoc rule-name clause-map))
      [(body ...+ rule-name:id)
       #'(body ...)]))

  (define-syntax (def-sig stx)
    (syntax-parse stx
      #:literal-sets [kernel-literals]
      [(_ name:id def:expr clause:expr)
       #:with sig-id  (format-id #'name "~a^" #'name)
       #:with unit-id (format-id #'name "~a@" #'name)
       #:do [(define def-cxt (syntax-local-make-definition-context))]
       #:with (define-values (var) e*)
       (local-expand #'def '() (list #'define-values) def-cxt)
       #:with var2 (internal-definition-context-splice-binding-identifier
                    def-cxt #'var)
       #`(begin
           (define-syntax name (list #'sig-id #'unit-id
                                     (make-clause-map (list #'clause))))
           (define-signature sig-id (var2))
           (define-unit unit-id (import) (export sig-id)
             (define var2 e*)))]))

  (def-sig ->>^ (define X 123) [(+ X X) add])

  (define-syntax (use-sig stx)
    (syntax-parse stx
      [(_ name:id super:id)
       #:with super-sig-id  (car (syntax-local-value #'super))
       #:with super-unit-id (cadr (syntax-local-value #'super))
       #:do [(define super-clause-map (caddr (syntax-local-value #'super)))]
       #:with (b ...) (get-clause-body super-clause-map 'add)
       #`(begin
           (define-unit name (import super-sig-id) (export)
             (define Y (begin b ...))
             Y)
           (invoke-unit
            (compound-unit (import) (export)
                           (link (([s : super-sig-id]) super-unit-id)
                                 (() =>>@ s)))))]))

  (use-sig =>>@ ->>^)

  

  )

;; (require 'experiment0)
;; (define-unit =>>@ (import ->>^) (export)
;;   (define Y (+ X 1)))


;; 親のシグネチャに含まれているはずの定義を参照できない問題

(module experiment racket
  (require (for-syntax racket racket/syntax
                       syntax/parse syntax/stx))  
  (begin-for-syntax
    (define (make-body red clause)
      (datum->syntax red (syntax->datum clause)))
    (struct reduction-desc
      (sig-id unit-id do-body clause) #:transparent))

  (define-syntax (def-super stx)
    (syntax-parse stx
      [(_ name:id var:id do-body:expr clause:expr)
       #:with sig-id  (format-id #'name "~a^" #'name)
       #:with unit-id (format-id #'name "~a@" #'name)
       #`(begin
           (define-syntax name
             (reduction-desc #'sig-id #'unit-id #'do-body #'clause))
           (define-signature sig-id (var))
           (define-unit unit-id (import) (export sig-id)
             do-body))]))

  (def-super --> X (define X 123) (+ X X))

  (define-syntax (def-sub stx)
    (syntax-parse stx
      [(_ name:id super-name:id)
       #:with sig-id  (format-id #'name "~a^" #'name)
       #:with unit-id (format-id #'name "~a@" #'name)
       #:do [(define super-desc (syntax-local-value #'super-name))]
       #:with super-sig-id (reduction-desc-sig-id super-desc)
       #:with super-clause (reduction-desc-clause super-desc)
       #`(begin
           (define-syntax name #'unit-id)
           (define-unit unit-id (import super-sig-id) (export)
             (define-signature M^
               ((define-syntaxes (#%reducer)
                  (λ (stx) #`(* 2 #,(make-body #'here #;#'name
                                                #'super-clause))))))
             (define-unit M@ (import) (export M^))
             (invoke-unit (compound-unit
                           (import) (export)
                           (link (([m : M^]) M@)
                                 (() (unit (import M^) (export)
                                       (#%reducer)) m))))))]))

  (def-sub ==> -->)

  (define-syntax (reducer-of stx)
    (syntax-parse stx
      [(_ name:id super-name:id)
       #:with unit-id (syntax-local-value #'name)
       #:with super-sig-id  (reduction-desc-sig-id
                             (syntax-local-value #'super-name))
       #:with super-unit-id (reduction-desc-unit-id
                             (syntax-local-value #'super-name))
       #'(invoke-unit (compound-unit
                       (import) (export)
                       (link (([s : super-sig-id]) super-unit-id)
                             (() unit-id s))))]))

  (reducer-of ==> -->)

  )

;; 上を生成するマクロ

(begin-for-syntax
  (struct reduction-desc (sig-id unit-id clause) #:transparent))

(define-for-syntax (make-body red clause)
  (datum->syntax red (syntax->datum clause)))

(define-syntax (def-super stx)
  (syntax-parse stx
    [(_ name:id field:id e:expr use:expr)
     #:with sig-id  (format-id #'name "~a^" #'name)
     #:with unit-id (format-id #'name "~a@" #'name)
     #'(begin
         (define-syntax name (reduction-desc #'sig-id #'unit-id #'use))
         (define-signature sig-id (field))
         (define-unit unit-id (import) (export sig-id)
           (define field e)))]))

(def-super -> X 123 (+ X X))

(define-syntax (def-sub stx)
  (syntax-parse stx
    [(_ name:id super:id)
     #:do [(define super-desc (syntax-local-value #'super))]
     #:with unit-id (format-id #'name "~a@" #'name)
     #:with super-sig-id  (reduction-desc-sig-id  super-desc)
     #`(define-unit unit-id (import super-sig-id) (export)
         (define-signature M^
           ((define-syntaxes (#%reducer)
              (λ (stx) #`(* 2 #,(make-body
                                  #'here ;#'name
                                  (reduction-desc-clause
                                   (syntax-local-value #'super))))))))
         (define-unit M@ (import) (export M^))
         (invoke-unit (compound-unit
                       (import) (export)
                       (link (([m : M^]) M@)
                             (() (unit (import M^) (export)
                                   (#%reducer)) m))))
         )]))

(def-sub => ->)

