#lang racket/signature

;; ----------------------------------------
;; Environment:

init-env ; -> Env
lookup-env ; Env Var -> Loc
update-env ; Env (Listof Var) (Listof Loc) -> Env
