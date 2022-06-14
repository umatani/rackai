#lang racket
(require "../reduction.rkt")

(define-signature I^ (I I2))
(define-signature J^ (J))
(define-signature X^ (X (struct Var (nam) #:constructor-name var)))
(define-signature Y^ (Y Y2))

(define-unit X@ (import) (export X^)
  (define X 200)
  (struct Var (nam) #:transparent #:constructor-name var))

(define-unit Y@ (import) (export Y^)
  (define Y -1000)
  (define Y2 20000))

(define-compound-unit XY@ (import) (export x y)
  (link (([x : X^]) X@)
        (([y : Y^]) Y@)))

(define-reduction (--> <+>) #:within-signatures [X^ Y^]
  [(cons a b) (<+> a b X Y Y2) 'add])

;; reducer->unitで生成したunitがexportするred^を受け取るunitと
;; 組み合わせる
(define-unit reducer1@ (import red^) (export)
  ((reducer +) (cons 5 6)))
(invoke-unit (compound-unit
              (import) (export)
              (link (([x : X^]) X@)
                    (([y : Y^]) Y@)
                    (([r : red^]) (reduction->unit -->) x y)
                    (() reducer1@ r))))

;; reducer->unitで生成したunitが返すreducerを直接使用
;; within-unitsとはcompound-unitで事前に組み合わせ
(define reducer2 (invoke-unit (compound-unit
                               (import) (export)
                               (link (([x : X^] [y : Y^]) XY@)
                                     (() (reduction->unit -->) x y)))))
((reducer2 +) (cons 3 4))


;; reducer-ofの引数に直接within-unitsを渡してreducerを生成
(define reducer3 (reducer-of --> #:within-units [X@ Y@]))
((reducer3 *) (cons 3 4))

;; reducer-ofの引数に渡すwithin-unitsは必要なsignaturesを供給していれば
;; 個数は一致してなくても良い．
(define reducer4 (reducer-of --> #:within-units [XY@]))
((reducer4 +) (cons 3 4))

(define-unit X/I@ (import I^) (export X^)
  (define X I)
  (struct Var (nam) #:transparent #:constructor-name var)
  (printf "X: ~a\n" X))

(define-unit Y/J@ (import J^) (export Y^)
  (define Y J)
  (define Y2 (* J J))
  (printf "Y: ~a\n" Y)
  (printf "Y2: ~a\n" Y2))

(define I  22)
(define I2 33)
(define J  44)
;; (invoke-unit (compose-unit X/I@ Y/J@)
;;              (import I^ J^))

;; reducer-of内部のinvoke-unitはcontextからII^, J^のbindingsを取得
(define reducer5 (reducer-of --> #:within-units [X/I@ Y/J@]))
((reducer5 /) (cons 3 4))


(define-reduction (==>) #:super (--> +)
  #:within-signatures [X^ Y^])

(reduction->unit ==>)
