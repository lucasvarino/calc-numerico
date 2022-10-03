#lang racket

; K constant
(define K 0.035871952)
; Initial time
(define t_i 0)
; End time
(define t_f 50)
; Initial temperature
(define θ_0 99)
; Ambient temperature
(define θ_m 27)
; Step
(define step
  (/ (- t_f t_i) (- t_f 1)))
;; Exact solution 
(define (exact-solution θ_0 θ_m K t)
  (+ (* (- θ_0 θ_m) (exp (* t (- K)))) θ_m))