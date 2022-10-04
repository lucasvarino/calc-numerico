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
;; Step
(define step
  (/ (- t_f t_i) (- t_f 1)))

;; Linspace
(define linspace
  (λ (start stop [num 50])
    (range start stop (/ stop num))))
; (linspace 0 50 50)

;; Exact solution 
(define (exact-solution t i result)
  (if (< i (length t))
         (exact-solution t (+ 1 i) (append result (list (+ (* (- θ_0 θ_m) (exp (* (list-ref t i) (- K)))) θ_m))))
         result))
; (exact-solution (linspace 0 50 50) 0 '())

;; Explict Euler
(define listOfResultExp (list θ_0))

(define (explicit-euler t_f t_i listOfResultExp)
  (if (< t_i (- t_f 1))
      (explicit-euler t_f (+ 1 t_i) (append listOfResultExp (list
                                                          (+ (* step (* (- K) (- (list-ref listOfResultExp t_i) θ_m))) (list-ref listOfResultExp t_i)))))
      listOfResultExp))

;(explicit-euler t_f t_i listOfResultExp)


;; Implicit Euler
(define listOfResultImp (list θ_0))

(define (implicit-euler t_f t_i listOfResultImp)
  (if (< t_i (- t_f 1))
      (implicit-euler t_f (+ 1 t_i) (append listOfResultImp (list
                                                             (/ (+ (list-ref listOfResultImp t_i) (* K step θ_m)) (+ 1 (* K step))))))
      listOfResultImp))

;(implicit-euler t_f t_i listOfResultExp)

