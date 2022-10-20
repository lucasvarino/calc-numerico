#lang racket
(define (exactSolution x c1 c2 ε)
  (+ (* c1 (expt (exp 1) (/ (- x) (sqrt ε)))) (* c2 (expt (exp 1) (/ x (sqrt ε)))) 1))