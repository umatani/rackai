#lang racket/signature

;; ----------------------------------------
;; Expand-time environment operations:

init-ξ   ; -> ξ
lookup-ξ ; ξ Nam -> AllTransform
extend-ξ ; ξ Nam AllTransform -> ξ
