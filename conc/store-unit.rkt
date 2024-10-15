#lang racket/unit
(require
 (only-in racket/match match)
 "../signatures.rkt"
 "../terms.rkt")

(import)
(export store^)

;; ----------------------------------------
;; Store

; init-store : → Store
(define (init-store) (Store 0 (make-immutable-hash)))

; lookup-store : Store Loc → (U Val Cont)
(define (lookup-store sto loc)
  (hash-ref (Store-tbl sto) loc))

; update-store : Store Loc (U Val Cont) → Store
(define (update-store sto loc v)
  (Store (Store-size sto)
         (hash-set (Store-tbl sto) loc v)))

; update-store* : Store (Listof Loc) (Listof (U Val Cont)) → Store
(define (update-store* sto locs vs)
  (foldl (λ (l u s) (update-store s l u))
         sto locs vs))

; alloc-loc : Symbol Store → (Values Loc Store)
;   - called from push-cont only
;   - a unique lbl is generated for each App and If form during parse
(define (alloc-loc lbl sto)
  (let ([size (Store-size sto)])
    (values (string->symbol (format "~a::~a" lbl size))
            (Store (add1 size) (Store-tbl sto)))))

; alloc-loc* : (Listof Nam) Store → (Values (Listof Loc) Store)
;   - for eval-time value binding
(define (alloc-loc* nams sto)
  (match nams
    ['() (values '() sto)]
    [(list nam nams ...)
     (let* ([size (Store-size sto)]
            [loc (string->symbol (format "~a:~a" nam size))])
       (let-values
           ([(locs sto′) (alloc-loc* nams (Store (add1 size) (Store-tbl sto)))])
         (values (cons loc locs) sto′)))]))
