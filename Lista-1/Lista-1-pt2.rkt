#lang racket

;; Alunos: Daniel Freitas e Lucas de Oliveira Varino

; ε = 0.1, 0.01, 0.001, 0.0001
(define ε 0.1)

(define c2
  (/ (- (expt (exp 1) (/ (- 1) (sqrt ε))) 1)
     (- (expt (exp 1) (/ 1 (sqrt ε))) (expt (exp 1) (/ (- 1) (sqrt ε))))))

(define c1
  (- (- 1) c2))

(define (exactSolution x)
  (+ (* c1 (expt (exp 1) (/ (- x) (sqrt ε)))) (* c2 (expt (exp 1) (/ x (sqrt ε)))) 1))


(define (algorithm-thomas a b c d)
  ;a, b, c are the column vectors for the compressed tridiagonal matrix, d is the right vector

  a)
