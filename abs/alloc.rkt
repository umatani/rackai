#lang racket
(require
 (only-in "../nondet.rkt"     results lift)
 (only-in "../mix.rkt"        define-mixed-unit)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../mult/units.rkt" [store@  super:store@]
                              [mstore@ super:mstore@]
                              [bind@   super:bind@]))
(provide store::fin-alloc@  store@
         mstore::fin-alloc@ mstore@
         syntax::fin-alloc@
         bind@)

(define-unit store::fin-alloc@
  (import)
  (export store^)

  (define (init-store    . args) (error "to be implemented"))
  (define (lookup-store  . args) (error "to be implemented"))
  (define (update-store  . args) (error "to be implemented"))
  (define (update-store* . args) (error "to be implemented"))


  (define all-loc (mutable-seteq))

  ; alloc-loc : Symbol Store -> (Values Loc Store)
  ;   - called only from push-cont
  ;   - a unique lbl is generated for each App and If form during parse
  (define (alloc-loc lbl st)
    (let ([loc (string->symbol (format "~a::" lbl))])
      (if (set-member? all-loc loc)
          (void) ;(printf "duplicate loc: ~a\n" loc)
          (set-add! all-loc loc))
      (values loc st)))

  ; alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store)
  ;   for eval-time value binding
  (define (alloc-loc* nams st)
    (match nams
      ['() (values '() st)]
      [(list nam1 nams ...)
       (let-values ([(locs _) (alloc-loc* nams st)])
         (let ([loc (string->symbol (format "~a:" nam1))])
           (if (set-member? all-loc loc)
               (void) ;(printf "duplicate loc: ~a\n" loc)
               (set-add! all-loc loc))
           (values (cons loc locs) st)))])))

(define-mixed-unit store@
  (import)
  (export store^)
  (inherit [super:store@ init-store lookup-store update-store update-store*]
           [store::fin-alloc@ alloc-loc alloc-loc*]))


(define-unit mstore::fin-alloc@
  (import)
  (export mstore^)

  (define (init-Σ        . args) (error "to be implemented"))
  (define (lookup-Σ      . args) (error "to be implemented"))
  (define (update-Σ      . args) (error "to be implemented"))

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  (define all-name  (mutable-seteq))
  (define all-𝓁     (mutable-set))

  ; alloc-name : Id Σ -> (Values Nam Σ)
  (define (alloc-name id Σ0)
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (let ([nam (string->symbol (format "~a:" nam))])
        (if (set-member? all-name nam)
            (void) ;(printf "duplicate name: ~a\n" nam)
            (set-add! all-name nam))
        (values nam Σ0))))

  ; alloc-𝓁 : Stx Σ -> (Values 𝓁 Σ)
  ;   - called only from push-κ
  ;   - stx is used in abs for ensuring finiteness of the domain
  (define (alloc-𝓁 stx Σ)
    (if (set-member? all-𝓁 stx)
        (void) ;(printf "duplicate 𝓁\n")
        (set-add! all-𝓁 stx))
    (values ;(𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (𝓁 stx) Σ)))

(define-mixed-unit mstore@
  (import)
  (export mstore^)
  (inherit [super:mstore@ init-Σ lookup-Σ update-Σ]
           [mstore::fin-alloc@ alloc-name alloc-𝓁]))


(define-unit syntax::fin-alloc@
  (import)
  (export syntax^)

  (define (empty-ctx      . args) (error "to be implemented"))
  (define (zip            . args) (error "to be implemented"))
  (define (unzip          . args) (error "to be implemented"))
  (define (in-hole        . args) (error "to be implemented"))
  (define (in-hole-stl    . args) (error "to be implemented"))
  (define (addremove      . args) (error "to be implemented"))
  (define (strip          . args) (error "to be implemented"))
  (define (add            . args) (error "to be implemented"))
  (define (add-stl        . args) (error "to be implemented"))
  (define (flip           . args) (error "to be implemented"))
  (define (flip-stl       . args) (error "to be implemented"))
  (define (subtract       . args) (error "to be implemented"))
  (define (prune          . args) (error "to be implemented"))
  (define (union          . args) (error "to be implemented"))
  (define (at-phase       . args) (error "to be implemented"))
  (define (update-ctx     . args) (error "to be implemented"))

  (define all-scope (mutable-seteq))

  ; alloc-scope : Symbol -> Scp
  (define (alloc-scope s)
    (if (set-member? all-scope s)
        (void) ;(printf "duplicate scope: ~a\n" s)
        (set-add! all-scope s))    #;(gensym s)
    s ;; TODO: s は nam (symbol) じゃなく Stx にすると精度向上
    )

  ; biggest-subset : Scps (Listof Scps) -> (Listof Scps)
  (define (biggest-subset scps_ref scpss)
    ;(printf "[biggest-subset] ~a ~a\n" scps_ref scpss)
    (let* ([matching (filter (λ (scps_bind)
                               (subset? scps_bind scps_ref))
                             scpss)]
           [sorted (sort matching > #:key set-count)])
      ;; The binding is ambiguous if the first scps in
      ;; `sorted` is not bigger than the others, or if
      ;; some scps in `sorted` is not a subset of the
      ;; first one.
      ;; --> サイズが最大なスコープセット全部を候補として返す
      (if (empty? sorted)
          (list (set))
          (let ([n (set-count (first sorted))])
            (for/list ([scps (in-list sorted)]
                       #:when (= (set-count scps) n))
              scps)))))

  ; binding-lookup : (Setof StoBind) Scps -> (Listof Nam)
  (define (binding-lookup sbs scps)
    ;(printf "[binding-lookup] ~a ~a\n" sbs scps)
    (map StoBind-nam (filter (λ (sb) (set=? (StoBind-scps sb) scps))
                             (set->list sbs)))))

(define-mixed-unit bind@
  (import  (only syntax^    binding-lookup biggest-subset at-phase)
           (only mstore^    lookup-Σ))
  (export  bind^)
  (inherit [super:bind@ bind])

  ; resolve : Ph Id Σ -> (SetM Nam)
  (define (resolve #:phase [ph #f] id Σ0)
    (match-let ([(Stx (Sym nam) ctx) id])
      ;(printf "resolve: ~a\n" nam)
      (let* ([sbss (filter set? (set->list (results (lookup-Σ Σ0 nam))))]
             ;[_ (printf "sbss: ~a\n" sbss)]
             [scpsss
              (let ([scpsss (map (λ (sbs)
                                   (set-map sbs (λ (sb) (StoBind-scps sb))))
                                 sbss)])
                (map remove-duplicates scpsss))]
             ;[_ (printf "scpsss: ~a\n" scpsss)]
             [scps_biggests (remove-duplicates
                             (append-map (λ (scpss)
                                           (biggest-subset
                                            (if ph (at-phase ctx ph) ctx)
                                            scpss))
                                         scpsss))]
             ;[_ (printf "scps_biggests: ~a\n" scps_biggests)]
             [nam_biggests
              (remove-duplicates
               (apply append
                      (for*/list ([sbs (in-list sbss)]
                                  [scps_biggest (in-list scps_biggests)])
                        (binding-lookup sbs scps_biggest))))])
        ;(printf "nam_biggests: ~a\n" nam_biggests)
        (let ([r (if (null? nam_biggests)
                     (set nam)
                     (list->set nam_biggests))])
          ;(printf "resolve done: ~a\n" r)
          (lift r)))))

  ; id=? : Ph Id Nam ξ Σ -> Boolean (same as conc/set)
  ;   ξ is non-#f only in full
  (define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
    (subset? (set nam) (results (resolve #:phase ph id Σ)))))
