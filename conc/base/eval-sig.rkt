#lang racket/signature

;; ----------------------------------------
;; Evaluating AST:

init-env ; -> Env
lookup-env ; Env Var -> Loc
update-env ; Env (Listof Var) (Listof Loc) -> Env

init-store ;  -> Store
lookup-store ; Store Loc -> (U Val Cont)
update-store ; Store Loc (U Val Cont) -> Store
update-store* ; Store (Listof Loc) (Listof (U Val Cont)) -> Store

alloc-loc ; Store -> (Values Loc Store)
;; for eval-time value binding
alloc-loc* ; (Listof Nam) Store -> (Values (Listof Loc) Store)

push-cont ; Store Cont -> (Values Loc Store)

;-->/store
--> ; State -> (Setof State)

eval/-->
eval     ; Ast -> Val
evaluate ; Ast -> Val
