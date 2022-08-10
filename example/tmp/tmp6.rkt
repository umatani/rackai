#lang racket
(require "../reduction.rkt")

;; ...をclause中のパターンで記述してある場合に，ちゃんとエスケープする方法
;; 結論 #'((... ...) clause) と書くとエスケープ
;; ↑自体が template の一部なので #'(... clause) ではないことに要注意

(define-reduction ->
  [`(,a ...) (apply + a) add])

(((reducer-of ->)) (list 1 2))

#;
(match (list 1 2)
  [`(,a ...) (apply + a)])

#;
(begin-for-syntax
  (define x (make-clause-map (list #'(... (`(,a ...) (apply + a) add)))))
  (println x)
  )
