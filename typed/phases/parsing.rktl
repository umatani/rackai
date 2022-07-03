;; ----------------------------------------
;; Syntax-object operations:

;; Similar to one-phase `add`, but must update context
;; at a given phase
(: add : Ph Stx Scp -> Stx)
(define (add ph stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (cast (add ph stx scp) Stx)
                   (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]))

(: add-stl : Ph Stl Scp -> Stl)
(define (add-stl ph stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (cast (add ph stx scp) Stx) (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(cons stx stl) (cons (cast (add ph stx scp) Stx)
                          (add-stl ph stl scp))]))


;; Similar to one-phase `flip`, but must update context
;; at a given phase
(: flip : Ph Stx Scp -> Stx)
(define (flip ph stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip ph stx scp) (flip-stl ph stl scp))
             (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]))

(: flip-stl : Ph Stl Scp -> Stl)
(define (flip-stl ph stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip ph stx scp) (flip-stl ph stl scp))
             (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(cons stx stl) (cons (flip ph stx scp) (flip-stl ph stl scp))]))

;; Recursively removes a set of scopes from a syntax object
;; at a given phase
(: prune : Ph Stx Scps -> Stx)
(define (prune ph stx scps_p)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
             (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]))

(: prune-stl : Ph Stl Scps -> Stl)
(define (prune-stl ph stl scps_p)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
             (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(cons stx stl) (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))]))

;; Updates the mapping of a `ctx` at a particular phase
(: update-ctx : Ctx Ph Scps -> Ctx)
(define (update-ctx ctx ph scps)
  (dict-set ctx ph scps))

;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
(: bind : Ph Σ Id Nam -> Σ)
(define (bind ph Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update tbl nam_1
                          (λ ([sbs : (U (Setof StoBind) Val ξ)])
                            (set-add (cast sbs (Setof StoBind))
                                     (StoBind (at-phase ctx_1 ph) nam)))
                          (λ () (ann (set) (Setof StoBind)))))))

(: at-phase : Ctx Ph -> Scps)
(define (at-phase ctx ph)
  (dict-ref ctx ph (λ () (set))))

;; Like the one-phase `resolve`, but at a particular phase
(: resolve : Ph Id Σ -> Nam)
(define (resolve ph id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (cast (lookup-Σ Σ0 nam) (Setof StoBind))]
           [scpss (map (λ ([sb : StoBind]) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;; ----------------------------------------
;; Simple parsing of already-expanded code

;; This parse is the same as the single-phase one, but with `ph`
;; threaded through to `resolve`
(: parse : Ph Stx Σ -> Ast)
(define (parse ph stx Σ)
  (define (id=? [nam : Nam]) : (-> Id Boolean)
    (λ (id) (eq? (resolve ph id Σ) nam)))

  (match stx
    ; (lambda (id ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'lambda)))
               ,(GenStx stl_ids _) ,stx_body) _)
     (Fun (map (λ ([id : Id]) (Var (resolve ph id Σ)))
               (cast stl_ids (Listof Id)))
          (parse ph stx_body Σ))]
    ; (let ([id stx_rhs] ...) stx_body)
    [(GenStx `(,(? Id? (? (id=? 'let)))
               ,(GenStx (? ProperStl?  stl_binds) _) ,stx_body) _)
     (let-values ([(stl_ids stl_rhs) (unzip stl_binds)])
       (App (Fun (map (λ ([id : Id]) (Var (resolve ph id Σ)))
                      (cast stl_ids (Listof Id)))
                 (parse ph stx_body Σ))
            (map (λ ([stx_rhs : Stx]) (parse ph stx_rhs Σ))
                 (cast stl_rhs (Listof Stx)))))]
    ; (quote stx)
    [(GenStx `(,(? Id? (? (id=? 'quote))) ,stx) _)
     (strip stx)]
    ; (syntax stx)
    [(GenStx `(,(? Id? (? (id=? 'syntax))) ,stx) _)
     stx]
    ; (#%app stx_fun stx_arg ...) stx-pair (cdr部もstx)であることに注意
    [(GenStx (cons (? Id? (? (id=? '#%app)))
                   (GenStx (cons stx_fun stl_args) _)) _)
     (App (parse ph stx_fun Σ) (parse* ph stl_args Σ))]
    ; (if stx stx stx)
    [(GenStx `(,(? Id? (? (id=? 'if))) ,stx_test ,stx_then ,stx_else) _)
     (If (parse ph stx_test Σ) (parse ph stx_then Σ) (parse ph stx_else Σ))]
    ; reference
    [(? Id? id) (Var (resolve ph id Σ))]
    ; literal
    [(GenStx (? Atom? atom) _) atom]))

(: parse* : Ph Stl Σ -> (Listof Ast))
(define (parse* ph stl Σ)
  (match stl
    ['() '()]
    [(cons stx stl) (cons (parse ph stx Σ) (parse* ph stl Σ))]
    [stx (list (parse ph (cast stx Stx) Σ))]))
