#lang racket

;; Alunos: Daniel Freitas e Lucas de Oliveira Varino

;; Graphics lib
(require graphics/graphics)
(require text-table)
(require text-table/utils)

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

;; Crank-Nicolson
(define listOfResultCN (list θ_0))

(define (crank-nicolson t_f t_i listOfResultCN)
  (if (< t_i (- t_f 1))
      (crank-nicolson t_f (+ 1 t_i) (append listOfResultCN (list
                                                             (/ (+ (/ (* (- K) step (- (list-ref listOfResultCN t_i) (* 2 θ_m))) 2) (list-ref listOfResultCN t_i)) (+ 1 (/ (* K step) 2))))))
      listOfResultCN))

;(crank-nicolson t_f t_i listOfResultCN)

;; Tabelas

(print-table
   #:->string (list ~a ; 1
                    (~r*) ; 2
                    (~r* #:notation 'exponential) ; 3
                    (~r* #:precision '(= 2)) ; 4 (good)
                    (~r* #:notation 'exponential #:precision '(= 2)) ; 5 (good)
                    (~r* #:min-width 10 #:pad-string ".")) ; 6
   #:align '(right ...)
   #:row-sep? '(#f #t #f ...)
   (cons
    '("1" "2" "3" "4 (good)" "5 (good)" "6")
    (transpose
    (make-list 6 `(header 1111.11 22.222 3333000.0 4440000000000.0 ,(sqrt 2))))))

          

;; Errors

(define (findError exact aprox result)
  (if (< (length result) (length aprox))
      (findError exact aprox (append result (list (abs (- (list-ref exact (length result)) (list-ref aprox (length result)))))))
      (apply max result)))

(display "Erro para o metodo Euler Explicito: ")
(findError (exact-solution (linspace 0 51 50) 0 '()) (explicit-euler t_f t_i listOfResultExp) '())
(display "Erro para o metodo Euler Implicito: ")
(findError (exact-solution (linspace 0 51 50) 0 '()) (implicit-euler t_f t_i listOfResultImp) '())
(display "Erro para o metodo Crank Nicolson: ")
(findError (exact-solution (linspace 0 51 50) 0 '()) (crank-nicolson t_f t_i listOfResultCN) '())