(: lookup-env : Env Var -> Loc)
(define (lookup-env env var) (hash-ref env var))

(: update-env : Env (Listof Var) (Listof Loc) -> Env)
(define (update-env env vars locs)
  (foldl (λ ([v : Var] [l : Loc] [e : Env]) (hash-set e v l))
         env vars locs))

(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store store loc)
  (hash-ref (Store-tbl store) loc))

(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store loc u)
  (Store (Store-size store)
         (hash-set (Store-tbl store) loc u)))

(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store locs us)
  (Store (Store-size store)
         (foldl (λ ([l : Loc]
                     [u : (U Val Cont)]
                     [t : (HashTable Loc (U Val Cont))])
                  (hash-set t l u))
                (Store-tbl store) locs us)))

(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc store)
  (let ([size (Store-size store)])
    (values (string->symbol (format "l~a" size))
            (Store (add1 size) (Store-tbl store)))))

;; for eval-time value binding
(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams store)
  (match nams
    ['() (values '() store)]
    [(list nam1 nams ...)
     (let* ([size (Store-size store)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (Store (add1 size) (Store-tbl store)))])
         (values (cons loc_0 locs_new) store_new)))]))

(: push-cont : Store Cont -> (Values Loc Store))
(define (push-cont store cont)
  (let-values ([(loc store_1) (alloc-loc store)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))
