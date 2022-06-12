#lang racket/signature

;; ----------------------------------------
;; Store:

init-store ;  -> Store
lookup-store ; Store Loc -> (U Val Cont)
update-store ; Store Loc (U Val Cont) -> Store
update-store* ; Store (Listof Loc) (Listof (U Val Cont)) -> Store

alloc-loc ; Store -> (Values Loc Store)
;; for eval-time value binding
alloc-loc* ; (Listof Nam) Store -> (Values (Listof Loc) Store)
