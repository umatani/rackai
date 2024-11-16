#lang racket/base
(require
 racket/unit
 (only-in racket/match                 match match-λ**)
 "../../interpreter.rkt"
 "../../signatures.rkt"
 (only-in "../../reduction.rkt"        define-reduction
                                       define-unit-from-reduction
                                       enable-tracing)
 (only-in "../../nondet.rkt"           do := <- pure lift enable-checkpoint)
 (only-in "../../mix.rkt"              define-mixed-unit inherit)
 (only-in "../../misc.rkt"             update-store* alloc-loc*)
 (only-in "../../set.rkt"              set ∅ set-add set→list ∪ for/set)
 (only-in "../../syntax.rkt"           stx→datum snoc zip unzip at-phase)
 "../../test/suites.rkt"
 "../../base/full/terms.rkt"
 (only-in "../../mult/full/units.rkt"  [syntax@ mult:syntax@]
                                       [bind@ mult:bind@]
                                       [parse@ mult:parse@] parser@)
 (only-in "../../mult/full/eval.rkt"   [--> mult:-->] define-eval-unit)
 (only-in "../../mult/full/expand.rkt" [==> mult:==>] define-expand-unit)
 (only-in "../full.rkt"                main-minus@)
 (only-in "domain.rkt"                 domain@))
(provide interp)


;;;; Syntax manipulation

(define-mixed-unit syntax@
  (import)
  (export syntax^)
  (inherit (mult:syntax@    empty-ctx in-hole
                            [mult:add add]
                            [mult:flip flip]
                            [mult:prune prune] proper-stl?))
  ;; add : Ph Stx Scp → Stx
  (define (add ph stx scp)
    (if (or (eq? stx 'stx-⊤)
            (eq? stx 'val-⊤))
      'stx-⊤
      (mult:add ph stx scp)))

  ;; flip : Ph Stx Scp → Stx
  (define (flip ph stx scp)
    (if (or (eq? stx 'stx-⊤)
            (eq? stx 'val-⊤))
      'stx-⊤
      (mult:flip ph stx scp)))

  ;; prune : Ph Stx Scps → Stx
  ;;   Recursively removes a set of scopes from a syntax object at a given phase
  (define (prune ph stx scps)
    (if (or (eq? stx 'stx-⊤)
            (eq? stx 'val-⊤))
      'stx-⊤
      (mult:prune ph stx scps)))

  )



;;;; Name resolution

(define-mixed-unit bind@
  (import
   (only mstore^    lookup-Σ all-nams))
  (export bind^)
  (inherit (mult:bind@    bind [mult:resolve resolve]))

  ;; resolve : Ph Id Σ → (SetM Nam)
  (define (resolve ph id Σ)
    (if (eq? id 'stx-⊤)
      (do nam  <- (lift (all-nams Σ))
          sb   <- (lookup-Σ Σ nam)
          nam′ <- (lift (StoBind-nam sb))
          (pure nam′))
      (mult:resolve ph id Σ)))

  )


;;;; Expander

(define-reduction (==> -->) #:super (mult:==> -->)
  #:import [(only common^    push-κ regist-vars)
            (only   misc^    lookup-κ)
            (only syntax^    empty-ctx add flip prune in-hole proper-stl?)
            (only    env^    init-env)
            (only  store^    init-store)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ alloc-name alloc-scope)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #;
  [(InEval (list stx '● _sto Σ̂)
           (ζ (Stxξ ph (Stx (Bool #f) _ctxᵢ) ξ)
              κ
              _Σ̂))
   #:when (or (equal? stx val-⊤)
              (equal? stx atom-⊤)
              (equal? stx stx-⊤))
   #:checkpoint (printf "ex-macapp-abs\n")
   (ζ (Stxξ ph stx ξ)
      κ
      Σ̂)
   ex-macapp-abs]

  ;; abstract values
  [(ζ (Stxξ ph 'stx-⊤ ξ)
      κ
      Σ̂)
   #:checkpoint (printf "ex-stx-⊤:\n")
   (ζ 'stx-⊤
      κ
      Σ̂)
   ex-stx-⊤])

(define-unit-from-reduction ex:red@ ==>)

(define-expand-unit expand@ ex:red@)


;;;; Parser

(define-mixed-unit parse@
  (import)
  (export  parse^)
  (inherit (mult:parse@ [mult:parse1 parse1] parse*))

  ; parse1 : Ph Stx Σ -> (SetM Ast)
  (define ((parse1 prs1 prs*) ph stx Σ)
    (if (eq? stx 'stx-⊤)
      (pure 'val-⊤) ;; TODO: ast-⊤?
      ((mult:parse1 prs1 prs*) ph stx Σ)))

  ; parse : Ph Stx Σ -> (SetM Ast)
  (define parse (parse1 parse1 parse*)))


;;;; Evaluator

(define-reduction (--> δ ==>) #:super (mult:--> δ ==>)
  #:import [(only common^    push-cont)
            (only     io^    all-ids)
            (only   misc^    lookup-cont lookup-val)
            (only syntax^    add flip prune)
            (only    env^    init-env lookup-env extend-env*)
            (only  store^    lookup-store update-store alloc-loc)
            (only   menv^    init-ξ lookup-ξ extend-ξ)
            (only mstore^    lookup-Σ update-Σ all-nams
                             alloc-name alloc-scope alloc-𝓁)
            (only   bind^    bind resolve)
            (only  parse^    parse)]

  #:do [;; resolve* : Ph (Listof Id) Σ → (SetM (Listof Nam))
        (define (resolve* ph ids Σ)
          (match ids
            ['pair-⊤ (pure (set→list (for/fold ([nams ∅])
                                               ([nam (all-nams Σ)])
                                       (do sb <- (lookup-Σ Σ nam)
                                           (∪ nams (StoBind-nam sb))))))]
            ['() (pure '())]
            [(cons id ids)
             (do nam  <- (resolve  ph id  Σ)
                 nams <- (resolve* ph ids Σ)
                 (pure (cons nam nams)))]))

        ]

  ;; (syntax-local-value <abs> _ ...)
  #;
  [`(,(Prim 'syntax-local-value _stx)
     ,(KApp′ `(,(? id? id) ,_val ...) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? id val-⊤) (equal? id atom-⊤)
              (equal? id stx-⊤))
   #:checkpoint (printf "ev-lval-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,val-⊤ ,cnt ,sto ,Σ̂)
   ev-lval-abs]

  ;; (syntax-local-identifier-as-binding <abs>)
  #;
  [`(,(Prim 'syntax-local-identifier-as-binding _stx)
     ,(KApp′ `(,(? id? id)) `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? id val-⊤)
              (equal? id atom-⊤)
              (equal? id stx-⊤)
              (and (Stx? id) (equal? (Stx-e id) sym-⊤)))
   #:checkpoint (printf "ev-lbinder-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,stx-⊤ ,cnt ,sto ,Σ̂)
   ev-lbinder-abs]

  ;; (syntax-local-bind-syntaxes <abs> <abs> <abs>)
  #;
  [`(,(Prim 'syntax-local-bind-syntaxes _stx)
     ,(KApp′ `(,ids ,rhs ,defs)
             `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (or (equal? ids list-⊤)
                  (and (Pair? ids) (Null? (Pair-d ids))
                       (let ([id (Pair-a ids)])
                         (or (equal? id val-⊤)
                             (equal? id atom-⊤)
                             (equal? id stx-⊤)
                             (and (Stx? id) (equal? (Stx-e id) sym-⊤))))))
              (or (equal? rhs (Bool #f))
                  (equal? rhs val-⊤)
                  (equal? rhs atom-⊤)
                  (equal? rhs stx-⊤))
              (or (equal? defs val-⊤)
                  (equal? defs atom-⊤)))
   #:checkpoint (printf "ev-slbs-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,list-⊤ ,cnt ,sto ,Σ̂)
   ev-slbs-abs]

  ;; create definition binding (for a variable)
  [`(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(pair-⊤ ,(Bool #f) ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   #:checkpoint (printf "ev-slbsv-abs\n")
   id′ <- (all-ids)
   `(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(,(Lst id′) ,(Bool #f) ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ev-slbsv-pair-⊤]

  ;; create macro definition binding
  [`(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(pair-⊤ ,(? stx? stx_arg) ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   #:checkpoint (printf "ev-slbsm\n")
   id′ <- (all-ids)
   `(,(Prim 'syntax-local-bind-syntaxes stx)
     ,(KApp′ `(,(Lst id′) ,stx_arg ,(Defs scp 𝓁))
             `(,ph ,env ,maybe-scpᵢ ,ξ) loc)
     ,sto ,Σ̂)
   ev-slbsm-pair-⊤]
  
  ;; (local-expand <abs> contextv idstops defs?) ;; TODO: check other args
  #;
  [`(,(Prim 'local-expand stx)
     ,(KApp′ `(,stx_arg ,_val_context ,_ids_stop ,_defs ...)
             `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:when (or (equal? stx_arg val-⊤)
              (equal? stx_arg atom-⊤)
              (equal? stx_arg stx-⊤))
   #:checkpoint (printf "ev-lexpand-abs\n")
   cnt <- (lookup-cont sto loc)
   `(,stx-⊤ ,cnt ,sto ,Σ̂)
   ev-lexpand-abs]

  ;; β (val-⊤ ...)
  [`(val-⊤
     ,(KApp′ _args `(,_ph ,_env ,_maybe-scpᵢ ,_ξ) loc)
     ,sto ,Σ̂)
   #:checkpoint (printf "ev-β-abs\n")
   cnt <- (lookup-cont sto loc)
   `(val-⊤ ,cnt ,sto ,Σ̂)
   ev-β-abs]

  ;; (if <abs> ...)
  #;
  [`(,(? val? val)
     ,(KIf _ast₁ ast₂ (list ph env maybe-scpᵢ ξ) loc)
     ,sto ,Σ̂)   
   #:when (or (equal? val val-⊤)
              (equal? val atom-⊤))
   #:checkpoint (printf "ev-if-abs-#f\n")
   cnt <- (lookup-cont sto loc)
   `(,(AstEnv ph ast₂ env maybe-scpᵢ ξ)
     ,cnt
     ,sto ,Σ̂)
   ev-if-abs-#f]
  )

(define-unit-from-reduction ev:red@ -->)

(define-eval-unit eval@ ev:red@)


;;;; Main

(define-values/invoke-unit
  (compound-unit/infer
   (import) (export domain^ run^)
   (link main-minus@
         domain@ syntax@ bind@ expand@ parse@ parser@ eval@))
  (import) (export domain^ run^))

(define interp (interpreter run δ α ≤ₐ))

;; run suites
(define (test)
  (run-suite 'core   interp)
  (run-suite 'phases interp)
  (run-suite 'full   interp)
  (run-suite 'finite interp))
