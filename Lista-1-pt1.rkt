#lang racket

;; Graphics lib
(require graphics/graphics)

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
(define (exact-solution t)
  (+ (* (- θ_0 θ_m) (exp (* t (- K)))) θ_m))
;; Explict Euler
(define listOfResult (list θ_0))

(define (explicit-euler t_f t_i listOfResult)
  (if (< t_i t_f)
      (explicit-euler t_f (+ 1 t_i) (append listOfResult (list
                                                          (+ (* step (* (- K) (- (list-ref listOfResult t_i) θ_m))) (list-ref listOfResult t_i)))))
      listOfResult))

(explicit-euler t_f t_i listOfResult)
