#lang racket/signature

;; ----------------------------------------
;; Expand-time stack operations:

init-Θ ; -> Θ

alloc-κ ; Θ -> (Values 𝓁 Θ)
lookup-κ ; Θ 𝓁 -> κ
update-κ ; Θ 𝓁 κ -> Θ
push-κ ; Θ κ -> (Values 𝓁 Θ)
