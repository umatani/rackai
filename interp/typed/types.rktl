;;;; Language

;; Use names for vars, symbols, locations, and scopes
(define-type Nam Symbol)

;; AST
(define-type Ast (U Val Var Fun App If))
(struct Var ([nam : Nam])                          #:transparent)
(struct Fun ([vars : (Listof Var)] [ast : Ast])    #:transparent)
(struct App ([rator : Ast] [rands : (Listof Ast)]) #:transparent)
(struct If  ([tst : Ast] [thn : Ast] [els : Ast])  #:transparent)
;; Value
(define-type Val (U VFun Atom (Pairof Val Val) Stx))
(struct VFun ([vars : (Listof Var)] [ast : Ast] [env : Env]) #:transparent)

;; Literal values
(define-type Atom (U Null Boolean Real Sym Prim #; 𝓁 #; Defs
                     ))
(struct Sym ([nam : Nam]) #:transparent)
(define-type Prim (U 'syntax-e
                     'datum->syntax
                     '+ '- '* '/ '< '= 'eq?
                     'cons 'car 'cdr 'list 'second 'third 'fourth
                     'syntax-local-value 'local-expand
                     'syntax-local-identifier-as-binding
                     'box 'unbox 'set-box!
                     'syntax-local-make-definition-context
                     'syntax-local-bind-syntaxes))
;(struct Defs ([scp : Scp] [𝓁 : 𝓁]) #:transparent)

;; Syntax objects (a subset of values)
(struct [A] GenStx ([e : A] [ctx : Ctx]) #:transparent)
(define-type Stx (U (GenStx Atom) (GenStx (Pairof Stx Stl)) (GenStx ProperStl)
                    Stxξ Hole (GenStx Hole)))
(define-type Stl (U Null Stx (Pairof Stx Stl) Hole)) ; syntax tail
(define-type Id (GenStx Sym) #:omit-define-syntaxes)
(define (Id [nam : Nam] [ctx : Ctx]) : Id (GenStx (Sym nam) ctx))
(define-type ProperStl (U Null (Pairof Stx ProperStl)))
(define-type Scps (Setof Scp))
(define-type Scp Nam)

;; Eval-time continuation, environment, and store
(define-type Loc Nam)
(define-type Env (HashTable Var Loc))
(struct Store ([size : Natural]
               [tbl : (HashTable Loc (U Val Cont))]) #:transparent)

(define-type Cont (U '• KApp KIf))
(struct KApp ([vals : (Listof Val)]
              [clos : (Listof Tm)]
              [loc : Loc]) #:transparent)
(struct KIf ([thn : Tm] [els : Tm] [loc : Loc]) #:transparent)
(define-type Tm (U Val Ser))
(define-type Ser (U AstEnv SApp SIf))
(struct AstEnv ([ast : Ast] [env : Env]) #:transparent)
(struct SApp ([vals : (Listof Val)]
              [clos : (Listof Tm)]) #:transparent)
(struct SIf ([tst : Tm] [thn : Tm] [els : Tm]) #:transparent)

(define-type State (List Tm Cont Store))

;; Expand-time environment
(define-type ξ (HashTable Nam AllTransform))
(define-type Transform (U TVar Val))
(struct TVar ([id : Id]) #:transparent)
(define-type AllTransform (U Transform TStop 'not-found))
(struct TStop ([all-transform : AllTransform]) #:transparent)

;; Expand-time store
(struct Σ ([size : Natural]
            [tbl : (HashTable (U Nam 𝓁) (U (Setof StoBind) Val ξ))])
  #:transparent)
(struct StoBind ([scps : Scps] [nam : Nam]) #:transparent)
(struct Θ ([size : Natural] [tbl : (HashTable 𝓁 κ)]) #:transparent)
(struct 𝓁 ([nam : Nam]) #:transparent)

;; Expand-time continuation
(struct Hole () #:transparent)
(define-type κ (U '• Mk-κ))
(struct Mk-κ ([stx : Stx] [ex? : Ex?] [𝓁 : 𝓁]) #:transparent)
(define-type Ex? (U '∘ '•))

;; Expand-time state (configuration)
(define-type ζ (U Zeta InEval) #:omit-define-syntaxes)
(struct Zeta ([stx : Stx] [ex? : Ex?] [κ : κ] [Θ : Θ] [Σ : Σ])
  #:transparent)
(struct InEval ([state : State] [ζ : ζ]) #:transparent)
(define-match-expander ζ
  (λ (stx) (syntax-case stx () [(_ . args) #'(Zeta . args)]))
  (λ (stx) (syntax-case stx () [(_ . args) #'(Zeta . args)])))

;; Additional predicates
(define-predicate Stx? Stx)
(define-predicate ProperStl? ProperStl)
(define-predicate Id? Id)
(define-predicate Atom? Atom)
(define-predicate Prim? Prim)
(define-predicate Val? Val)
(define-predicate State? State)
(define-predicate Nam? Nam)

