;; ----------------------------------------
;; Implementation of primitives:

(define (plus . [ns : Real *]) : Real (apply + ns))
(define (minus [n : Real] . [ns : Real *]) : Real (apply - n ns))
(define (times . [ns : Real *]) : Real (apply * ns))
(define (div [n : Real] . [ns : Real *]) : Real (apply / n ns))
(define (less-than [n1 : Real] [n2 : Real] . [ns : Real *])
  (apply < n1 n2 ns))
(define (num-eq [n1 : Real] [n2 : Real] . [ns : Real *]) : Boolean
  (apply = n1 n2 ns))
(define (sym-eq [sym1 : Sym] [sym2 : Sym]) : Boolean
  (match* (sym1 sym2)
    [((Sym nam1) (Sym nam2)) (eq? nam1 nam2)]))

(: δ : Prim (Listof Val) -> Val)
(define (δ p vs)
  (match (cons p vs)
    [`(+ ,(? real? ns) ...)
     (apply plus (cast ns (Listof Real)))]
    [`(- ,(? real? n) ,(? real? ns) ...)
     (apply minus n (cast ns (Listof Real)))]
    [`(* ,(? real? ns) ...)
     (apply times (cast ns (Listof Real)))]
    [`(/ ,(? real? n) ,(? real? ns) ...)
     (apply div n (cast ns (Listof Real)))]
    [`(< ,(? real? n1) ,(? real? n2) ,(? real? ns) ...)
     (apply less-than n1 n2 (cast ns (Listof Real)))]
    [`(= ,(? real? n1) ,(? real? n2) ,(? real? ns) ...)
     (apply num-eq n1 n2 (cast ns (Listof Real)))]

    [`(eq? ,(? Sym? s1) ,(? Sym? s2)) (sym-eq s1 s2)]

    [`(cons ,v1 ,v2) (cons v1 v2)]
    [`(car ,(cons v1 _)) v1]
    [`(cdr ,(cons _ v2)) v2]

    [`(list) '()]
    [`(list ,v1 ,vs ...) (δ 'cons (list v1 (δ 'list vs)))]
    [`(second (,_ ,v2 ,_ ...)) v2]
    [`(third  (,_ ,_ ,v3 ,_ ...)) v3]
    [`(fourth (,_ ,_ ,_ ,v4 ,_ ...)) v4]

    [`(syntax-e ,(GenStx e _)) e]
    [`(datum->syntax ,_ ,(? Stx? stx)) stx]
    [`(datum->syntax ,(and stx0 (GenStx _ ctx_0)) (,v1 ,vs ...))
     (GenStx `(,(cast (δ 'datum->syntax `(,stx0 ,v1)) Stx)
               ,@(cast (δ 'syntax-e `(,(δ 'datum->syntax `(,stx0 ,vs))))
                       Stl))
             ctx_0)]
    [`(datum->syntax ,(GenStx _ ctx) ,(? Atom? atom))
     (GenStx atom ctx)]))
