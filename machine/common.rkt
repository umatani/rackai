#lang racket ;scheme
(require redex/reduction-semantics
         (rename-in racket/base
           [read r:read]
           [eval r:eval]
           [expand r:expand]))
(provide ∈ find ext
         define-helpers define-runner
         r:read r:eval r:expand
         run-ex run-examples run-all-examples)


(define-language REDEX)

(define-relation REDEX
  ∈ ⊆ any × (any ...)
  [(∈ any (any_0 ... any any_1 ...))])

(define-metafunction REDEX
  find : ([any any] ...) any  -> any
  [(find (_ ... [any_k any_v] _ ...) any_k) any_v])

(define-metafunction REDEX
  ext : ([any any] ...) (any any) ... -> any
  [(ext (any ...) (any_k any_v) ...)
   ([any_k any_v] ... any ...)])


(define-syntax-rule (define-helpers L empty-ctx reader printer)
  (...
   (begin
     (define-metafunction L
       reader : any -> stx
       [(reader nam)       (Stx (Sym nam) empty-ctx)]
       [(reader atom)      (Stx atom      empty-ctx)]
       [(reader (any_1 any_2 ...))
        (Stx (Cons (reader any_1) val) empty-ctx)
        (where (Stx val ctx) (reader (any_2 ...)))])
     (define-metafunction L
       printer : val -> any
       [(printer (Fun (var ...) ast)) (Fun (var ...) ast)]
       [(printer (Fun (var ...) ast env)) (Fun (var ...) ast)]
       [(printer (Sym nam)) nam]
       [(printer atom) atom]
       [(printer (Stx val _)) ,(vector 'Stx (term (printer val)))]
       [(printer (Cons val_1 val_2)) ,(cons (term (printer val_1))
                                            (term (printer val_2)))]))))

(define-syntax-rule (define-runner run reader expander stripper printer
                      evaluate parser)
  (define run
    (lambda (form mode)
      (let ([stx (term (reader ,form))])
        (if (eq? mode 'read)
            stx
            (let ([stx2 (term (expander ,stx))])
              (if (eq? mode 'expand)
                  (term (printer (stripper ,stx2)))
                  (let ([ast (term (parser ,stx2))])
                    (if (eq? mode 'parse)
                        ast
                        (let ([ast2 (term (evaluate ,ast))])
                          (if (eq? mode 'eval)
                              (term (printer ,ast2))
                              (error 'run "unknown mode: ~e" mode))))))))))))


;;;; Example runner

(define fail-count (make-parameter #f))

(define (ex-runner run example mode)
  (printf "~a: " (car example))
  (println
   (case mode
     [(raw) (r:eval (cadr example))]
     [(check)
      (fail-count (or (fail-count) 0))
      (let ([result (equal? (run (cadr example) 'eval)
                            (r:eval (cadr example)))])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run (cadr example) mode)])))

(define (run-ex run examples name [mode 'check])
  (let ([example (cadr (assoc name examples))])
    (ex-runner run example mode)))

(define (run-examples run examples [mode 'check])
  (for ([example (in-list examples)])
    (ex-runner run example mode)))

(define ((run-all-examples all-runs all-examples) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 #f)])
    (for ([run (in-list all-runs)]
          [idx (in-naturals 1)])
      (printf "[~a]\n" (car run))
      (for ([i (in-range idx)]
            [examples (in-list all-examples)])
        (run-examples (cadr run) examples mode)))
    (when (fail-count)
      (printf "\nfail-count: ~a\n" (fail-count)))))


;; (define fail-count (make-parameter #f))

;; (define (run-examples run examples mode)
;;   (for ([example (in-list examples)])
;;     (printf "~a: " (car example))
;;     (println
;;      (case mode
;;        [(raw) (r:eval (cadr example))]
;;        [(check)
;;         (fail-count (or (fail-count) 0))
;;         (let ([result (equal? (run (cadr example) 'eval)
;;                               (r:eval (cadr example)))])
;;           (unless result
;;             (fail-count (+ (fail-count) 1)))
;;           result)]
;;        [else (run (cadr example) mode)]))))

;; (define ((run-all-examples all-runs all-examples) [mode 'check])
;;   (parameterize ([fail-count (if (eq? mode 'check) 0 #f)])
;;     (for ([run (in-list all-runs)]
;;           [idx (in-naturals 1)])
;;       (printf "[~a]\n" (car run))
;;       (for ([i (in-range idx)]
;;             [examples (in-list all-examples)])
;;         (run-examples (cadr run) examples mode)))
;;     (when (fail-count)
;;       (printf "\nfail-count: ~a\n" (fail-count)))))
