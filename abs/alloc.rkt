#lang racket/base
(require
 racket/unit
 (only-in racket/list         empty? first)
 (only-in racket/match        match match-let)
 (prefix-in r: racket/set)
 (only-in "../nondet.rkt"     results lift)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 (only-in "../set.rkt"        ∅ set=? ⊆ set-size set→list)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../mult/units.rkt" [ store@  mult:store@]
                              [mstore@ mult:mstore@]))
(provide store@ mstore@ biggest-subset binding-lookup)


(define-mixed-unit store@
  (import)
  (export  store^)
  (inherit [mult:store@ init-store lookup-store update-store update-store*])

  (define all-locs (r:mutable-seteq))

  ; alloc-loc : Symbol Store -> (Values Loc Store)
  ;   - called only from push-cont
  ;   - a unique lbl is generated for each App and If form during parse
  (define (alloc-loc lbl st)
    (let ([loc (string->symbol (format "~a::" lbl))])
      (if (r:set-member? all-locs loc)
        (void) ;(printf "duplicate loc: ~a\n" loc)
        (r:set-add! all-locs loc))
      (values loc st)))

  ; alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store)
  ;   for eval-time value binding
  (define (alloc-loc* nams st)
    (match nams
      ['() (values '() st)]
      [(list nam1 nams ...)
       (let-values ([(locs _) (alloc-loc* nams st)])
         (let ([loc (string->symbol (format "~a:" nam1))])
           (if (r:set-member? all-locs loc)
             (void) ;(printf "duplicate loc: ~a\n" loc)
             (r:set-add! all-locs loc))
           (values (cons loc locs) st)))])))


(define-mixed-unit mstore@
  (import)
  (export  mstore^)
  (inherit [mult:mstore@          init-Σ lookup-Σ update-Σ])

  ;; ----------------------------------------
  ;; Alloc name & scope helpers for expander:

  (define all-name  (r:mutable-seteq))
  (define all-scope (r:mutable-seteq))
  (define all-𝓁     (r:mutable-set))

  ;; alloc-name : Id Σ → (Values Nam Σ)
  (define (alloc-name id Σ0)
    (match-let ([(Stx (Sym nam) _) id]
                [(Σ size tbl) Σ0])
      (let ([nam (string->symbol (format "~a:" nam))])
        (if (r:set-member? all-name nam)
          (void) ;(printf "duplicate name: ~a\n" nam)
          (r:set-add! all-name nam))
        (values nam Σ0))))

  ;; alloc-scope : Symbol Σ → (Values Scp Σ)
  (define (alloc-scope s Σ0)
    (if (r:set-member? all-scope s)
      (void) ;(printf "duplicate scope: ~a\n" s)
      (r:set-add! all-scope s))    #;(gensym s)
    (values s Σ0) ;; TODO: s は nam (symbol) じゃなく Stx にすると精度向上
    )

  ;; alloc-𝓁 : Stx Σ -> (Values 𝓁 Σ)
  ;;   - called only from push-κ
  ;;   - stx is used in abs for ensuring finiteness of the domain
  (define (alloc-𝓁 stx Σ)
    (if (r:set-member? all-𝓁 stx)
      (void) ;(printf "duplicate 𝓁\n")
      (r:set-add! all-𝓁 stx))
    (values ;(𝓁 (string->symbol (format "𝓁:~a:~a" stx size)))
     (𝓁 stx) Σ)))


; biggest-subset : Scps (Listof Scps) → (Listof Scps)
(define (biggest-subset scps_ref scpss)
  ;(printf "[biggest-subset] ~a ~a\n" scps_ref scpss)
  (let* ([matching (filter (λ (scps_bind)
                             (⊆ scps_bind scps_ref))
                           scpss)]
         [sorted (sort matching > #:key set-size)])
    ;; The binding is ambiguous if the first scps in
    ;; `sorted` is not bigger than the others, or if
    ;; some scps in `sorted` is not a subset of the
    ;; first one.
    ;; --> サイズが最大なスコープセット全部を候補として返す
    (if (empty? sorted)
      (list ∅)
      (let ([n (set-size (first sorted))])
        (for/list ([scps (in-list sorted)]
                   #:when (= (set-size scps) n))
          scps)))))

; binding-lookup : (Setof StoBind) Scps → (Listof Nam)
(define (binding-lookup sbs scps)
  ;(printf "[binding-lookup] ~a ~a\n" sbs scps)
  (map StoBind-nam (filter (λ (sb) (set=? (StoBind-scps sb) scps))
                           (set→list sbs))))
