#lang racket
(require (only-in "../nondet.rkt" do pure break)
         (only-in racket [eval r:eval])
         "struct.rkt")
(provide (all-defined-out))

;;;; reader & printer

(define-syntax-rule (define-helpers empty-ctx reader printer)
  (...
   (begin
     (define reader
       (letrec ([read-stx
                 (λ (x)
                   (match x
                     [(? Prim?) (GenStx x empty-ctx)]
                     [(? symbol?) (GenStx (Sym x) empty-ctx)]
                     [(? Atom?) (GenStx x empty-ctx)]
                     [(? pair?)
                      (let ([stl (read-stl x)])
                        (match stl
                          ['() (GenStx '() empty-ctx)]
                          [(? Stx? stx) stx]
                          [(? pair? stl) (GenStx stl empty-ctx)]))]
                     [_ (error 'reader "not supported: ~a" x)]))]
                [read-stl
                 (λ (xs)
                   (match xs
                     ['() '()]
                     [(cons y ys)
                      (cons (read-stx y) (read-stl ys))]
                     [(? Atom? atom) (read-stx atom)]))])
         read-stx))

     (define (printer val)
       (match val
         [(? (λ (v) (or (null? v) (Prim? v) (real? v) (boolean? v)))) val]
         [(VFun `(,(Var nams) ...) ast env) `(VFun ,@nams)]
         [(Sym nam) nam]
         [(cons v1 v2) (cons (printer v1) (printer v2))]
         [(GenStx a c) (vector 'Stx (printer a))]
         ;[(𝓁 nam) nam]
         ;[(Defs _ _) '(Defs)]
         )))))

;;;; runner

(define-syntax-rule (define-runner run reader printer expander parser evaluater)
  (define (run form mode)
    (set-first
     (cdr (do stx := (reader form)
              #:failif (eq? mode 'read) stx
              (cons stx2 Σ2) := (expander stx)
              #:failif (eq? mode 'expand) stx2
              ast := (parser stx2 Σ2)
              #:failif (eq? mode 'parse) ast
              ast2 := (evaluater ast)
              #:failif (eq? mode 'eval) (printer ast2)
              (error 'run "unknown mode: ~e" mode))))))


(define-syntax-rule (define-runner2 run reader printer expander parser evaluater)
  (define (run form mode)
    (if (eq? mode 'none)
        form
        (let ([stx (reader form)])
          (if (eq? mode 'read)
              stx
              (let-values ([(stx2 Σ2) (expander stx)])
                (if (eq? mode 'expand)
                    stx2
                    (let ([ast (parser stx2 Σ2)])
                      (if (eq? mode 'parse)
                          ast
                          (let ([ast2 (evaluater ast)])
                            (if (eq? mode 'eval)
                                (printer ast2)
                                (error 'run "unknown mode: ~e" mode))))))))))))

;;;; Example runner

(define fail-count (make-parameter -1))

(define (ex-runner run example mode)
  (printf "~a: " (car example))
  (println
   (case mode
     [(raw) (first (call-with-values (λ () (r:eval (cadr example)))
                                     (λ args args)))]
     [(check)
      (fail-count (if (< (fail-count) 0) 0 (fail-count)))
      (let ([result (equal? (run (cadr example) 'eval)
                            (first (call-with-values
                                    (λ () (r:eval (cadr example)))
                                    (λ args args))))])
        (unless result
          (fail-count (+ (fail-count) 1)))
        result)]
     [else (run (cadr example) mode)])))

(define (run-ex run examples name [mode 'check])
  (let ([example (assoc name examples)])
    (when example (ex-runner run example mode))))

(define (run-examples run examples [mode 'check])
  (for ([example (in-list examples)])
    (ex-runner run example mode)))

(define ((run-all-examples all-runs all-examples) [mode 'check])
  (parameterize ([fail-count (if (eq? mode 'check) 0 -1)])
    (for ([run (in-list all-runs)]
          [idx (in-naturals 1)])
      (printf "[~a]\n" (car run))
      (for ([i (in-range idx)]
            [examples (in-list all-examples)])
        (run-examples (cadr run) examples mode)))
    (when (>= (fail-count) 0)
      (printf "\nfail-count: ~a\n" (fail-count)))))
