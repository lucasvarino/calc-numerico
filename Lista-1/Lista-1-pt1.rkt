#lang racket

;; Alunos: Daniel Freitas e Lucas de Oliveira Varino

;; Libs
(require graphics/graphics)
(require text-table)
(require text-table/utils)
(require math/bigfloat)
(require plot)

;; Precision
; 64-bits
(bf-precision 64)
#|
Erro para o metodo Euler Explicito: 0.5029193064673478
Erro para o metodo Euler Implicito: 0.4668773416941292
Erro para o metodo Crank Nicolson:  0.013553951180689694
|#
; 32-bits
;(bf-precision 32)
#|
Erro para o metodo Euler Explicito: 0.5029192864894867
Erro para o metodo Euler Implicito: 0.4668773114681244
Erro para o metodo Crank Nicolson:  0.0135539472103118
|#


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
  (bigfloat->flonum (bf (/ (- t_f t_i) (- t_f 1)))))

;; Linspace
(define linspace
  (λ (start stop [num 50])
    (range start stop (bigfloat->flonum (bf (/ stop num))))))
; (linspace 0 51 50)

;; Exact solution 
(define (exact-solution t i result)
  (if (< i (length t))
      (exact-solution t (+ 1 i) (append result (list 
                                                (bigfloat->flonum (bf (+ (* (- θ_0 θ_m) (exp (* (list-ref t i) (- K)))) θ_m))))))
      result))
; (exact-solution (linspace 0 50 50) 0 '())

;; Explict Euler
(define listOfResultExp (list θ_0))

(define (explicit-euler t_f t_i listOfResultExp)
  (if (< t_i (- t_f 1))
      (explicit-euler t_f (+ 1 t_i) (append listOfResultExp (list
                                                             (bigfloat->flonum (bf (+ (* step (* (- K) (- (list-ref listOfResultExp t_i) θ_m))) (list-ref listOfResultExp t_i)))))))
      listOfResultExp))

;(explicit-euler t_f t_i listOfResultExp)


;; Implicit Euler
(define listOfResultImp (list θ_0))

(define (implicit-euler t_f t_i listOfResultImp)
  (if (< t_i (- t_f 1))
      (implicit-euler t_f (+ 1 t_i) (append listOfResultImp (list
                                                             (bigfloat->flonum (bf (/ (+ (list-ref listOfResultImp t_i) (* K step θ_m)) (+ 1 (* K step))))))))
      listOfResultImp))

;(implicit-euler t_f t_i listOfResultExp)

;; Crank-Nicolson
(define listOfResultCN (list (bigfloat->flonum (bf θ_0))))

(define (crank-nicolson t_f t_i listOfResultCN)
  (if (< t_i (- t_f 1))
      (crank-nicolson t_f (+ 1 t_i) (append listOfResultCN (list
                                                            (bigfloat->flonum (bf (/ (+ (/ (* (- K) step (- (list-ref listOfResultCN t_i) (* 2 θ_m))) 2) (list-ref listOfResultCN t_i)) (+ 1 (/ (* K step) 2))))))))
      listOfResultCN))

;(crank-nicolson t_f t_i listOfResultCN)

;; Tables
(display "\n======\nTabela Comparação de metodos\n======\n")
(print-table
 #:->string (list (~r* #:precision '(= 10)))
 #:align '(center ...)
 #:row-sep? #t
 #:border-style 'double ;'latex ;<- Se quiser que saia em latex 
 (cons
  '("Exato" "Euler Explícito" "Euler Implícito" "Crank Nicolson")
  (transpose
   (list (exact-solution (linspace 0 51 50) 0 '())
         (explicit-euler t_f t_i listOfResultExp) 
         (implicit-euler t_f t_i listOfResultImp)
         (crank-nicolson t_f t_i listOfResultCN)))))



(define (convergence-rate t_f t_i listOfResultExp listOfResultImp listOfResultCN)
  (if (< t_i (- t_f 1))
      (convergence-rate t_f (+ 1 t_i) (append listOfResultExp (list
                                                               (bigfloat->flonum (bf (+ (* step (* (- K) (- (list-ref listOfResultExp t_i) θ_m))) (list-ref listOfResultExp t_i)))))) 
                        (append listOfResultImp (list
                                                 (bigfloat->flonum (bf (/ (+ (list-ref listOfResultImp t_i) (* K step θ_m)) (+ 1 (* K step)))))))
                        (append listOfResultCN (list
                                                (bigfloat->flonum (bf (/ (+ (/ (* (- K) step (- (list-ref listOfResultCN t_i) (* 2 θ_m))) 2) (list-ref listOfResultCN t_i)) (+ 1 (/ (* K step) 2))))))))
      (list (list-ref listOfResultExp (- t_f 1)) (list-ref listOfResultImp (- t_f 1)) (list-ref listOfResultCN (- t_f 1)))))

;; Errors

(define (findError exact aprox result)
  (if (< (length result) (length aprox))
      (findError exact aprox (append result (list (abs (bigfloat->flonum (bf (- (list-ref exact (length result)) (list-ref aprox (length result)))))))))
      (apply max result)))

(display "Erro para o metodo Euler Explicito: ")
(bigfloat->flonum (bf (findError (exact-solution (linspace 0 51 50) 0 '()) (explicit-euler t_f t_i listOfResultExp) '())))
(display "Erro para o metodo Euler Implicito: ")
(bigfloat->flonum (bf (findError (exact-solution (linspace 0 51 50) 0 '()) (implicit-euler t_f t_i listOfResultImp) '())))
(display "Erro para o metodo Crank Nicolson:  ")
(bigfloat->flonum (bf (findError (exact-solution (linspace 0 51 50) 0 '()) (crank-nicolson t_f t_i listOfResultCN) '())))

(display "convergencia: ")
(convergence-rate t_f t_i listOfResultExp listOfResultImp listOfResultCN)