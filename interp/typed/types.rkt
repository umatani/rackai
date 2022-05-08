#lang typed/racket

(provide (all-defined-out))

;;;; Language

;; Executable AST and values:
(define-type Ast (U Var Val Fun App If))
(struct Var ([nam : Nam]) #:transparent)
(struct Fun ([vars : (Listof Var)] [ast : Ast]) #:transparent)
(struct App ([rator : Ast] [rands : (Listof Ast)]) #:transparent)
(struct If ([tst : Ast] [thn : Ast] [els : Ast]) #:transparent)

(define-type Val (U VFun Atom (Pairof Val Val) Stx))
(struct VFun ([vars : (Listof Var)] [ast : Ast] [env : Env]) #:transparent)

;; Syntax objects (a subset of values):
(struct [A] GenStx ([e : A] [ctx : Ctx]) #:transparent)
(define-type Stx (U (GenStx Atom) (GenStx (Pairof Stx Stl)) (GenStx ProperStl)
                    Stxξ Hole (GenStx Hole)))
(define-predicate Stx? Stx)
(struct Stxξ ([stx : Stx] [ξ : ξ]) #:transparent)
(define-type Stl (U Null Stx (Pairof Stx Stl) Hole)) ; syntax tail
(define-type Id (GenStx Sym) #:omit-define-syntaxes)
(define (Id [nam : Nam] [scps : Scps]) : Id (GenStx (Sym nam) scps))
(define-predicate Id? Id)
(define-type ProperStl (U Null (Pairof Stx ProperStl)))
(define-predicate ProperStl? ProperStl)
(define-type Ctx Scps)
(define-type Scps (Setof Scp))
(define-type Scp Nam)

;; Literal values:
(define-type Atom (U Null Sym Prim Real Boolean 𝓁 Defs))
(define-predicate Atom? Atom)
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
(define-predicate Prim? Prim)
(struct Defs ([scp : Scp] [𝓁 : 𝓁]) #:transparent)

;; Eval-time continuation, environment, and store
(define-type Cont (U '• KApp KIf))
(struct KApp ([vals : (Listof Val)]
              [clos : (Listof Clo)]
              [loc : Loc]) #:transparent)
(struct KIf ([thn : Clo] [els : Clo] [loc : Loc]) #:transparent)
(define-type Env (HashTable Var Loc))
(define-type Clo (U Val Ser))
(define-type Ser (U AstEnv SApp SIf))
(struct AstEnv ([ast : Ast] [env : Env]) #:transparent)
(struct SApp ([vals : (Listof Val)]
              [clos : (Listof Clo)]) #:transparent)
(struct SIf ([tst : Clo] [thn : Clo] [els : Clo]) #:transparent)
(define-type State (List Clo Cont Store))
(define-predicate State? State)
(struct Store ([size : Natural]
               [tbl : (HashTable Loc (U Val Cont))]) #:transparent)
(define-type Loc Nam)

;; Expand-time environment:
(define-type ξ (HashTable Nam AllTransform))
(define-type Transform (U TVar Val))
(struct TVar ([id : Id]) #:transparent)
(define-type AllTransform (U Transform TStop 'not-found))
(struct TStop ([all-transform : AllTransform]) #:transparent)

  ;; Expand-time store:
(struct Σ ([size : Natural]
            [tbl : (HashTable (U Nam 𝓁) (U (Setof StoBind) Val ξ))])
  #:transparent)
(struct StoBind ([scps : Scps] [nam : Nam]) #:transparent)
(struct Θ ([size : Natural] [tbl : (HashTable 𝓁 κ)]) #:transparent)
(struct 𝓁 ([nam : Nam]) #:transparent)

;; Expand-time continuation:
(struct Hole () #:transparent)
(define-type κ (U '• Mk-κ))
(struct Mk-κ ([stx : Stx] [ex? : Ex?] [𝓁 : 𝓁]) #:transparent)
(define-type Ex? (U '∘ '•))

;; Expand-time state (configuration):
(define-type ζ (U Zeta InEval) #:omit-define-syntaxes)
(struct Zeta ([stx : Stx] [ex? : Ex?] [κ : κ] [Θ : Θ] [Σ : Σ])
  #:transparent ;#:constructor-name ζ
  )
(define-match-expander ζ
  (λ (stx) (syntax-case stx () [(_ . args) #'(Zeta . args)]))
  (λ (stx) (syntax-case stx () [(_ . args) #'(Zeta . args)])))
(struct InEval ([state : State] [ζ : ζ]) #:transparent)

;; Use names for vars, locations, and scopes
(define-type Nam Symbol)

;; Additional predicates
(define-predicate Nam? Nam)
(define-predicate Val? Val)

