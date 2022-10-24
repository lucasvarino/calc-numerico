#lang racket

;; Alunos: Daniel Freitas e Lucas de Oliveira Varino

; ε = 0.1, 0.01, 0.001, 0.0001
(define ε 0.1)

(define c2
  (/ (- (expt (exp 1) (/ (- 1) (sqrt ε))) 1)
     (- (expt (exp 1) (/ 1 (sqrt ε))) (expt (exp 1) (/ (- 1) (sqrt ε))))))

(define c1
  (- (- 1) c2))

;; Solucao Exata
(define (exactSolution x)
  (+ (* c1 (expt (exp 1) (/ (- x) (sqrt ε)))) (* c2 (expt (exp 1) (/ x (sqrt ε)))) 1))


;; Aux

(define (forListC_ a b c c_ n i)
  (if (< i (- n 1))
      (forListC_ a b c (append c_ (list (/ (list-ref c i) (- (list-ref b i) (* (list-ref c_ (- i 1)) (list-ref a (- i 1))))))) n (add1 i))
      c_))

(define (forListX c_1 d_1 rg x i)
  (if (not (null? rg))
      (forListX c_1 d_1 (cdr rg) (list (append x (map (lambda (number)
                                                        (+ (- (list-ref d_1 i) (* (list-ref c_1 i) (list-ref x 0))) number))
                                                      x))) (car rg))
      x))

;; Algoritmo de Thomas
(define (algorithm-thomas a b c d)
  (define c_ (list (/ (car c ) (car b))))
  (define d_ (list (/ (car d ) (car b))))

  (define c_1 (forListC_ a b c c_ (length d) 1))

  (define d_1 (append d_ (list (/ (- (list-ref d (- (length d) 1)) (* (list-ref d_ (- (- (length d) 2) 1)) (list-ref a (- (- (length d) 2) 1))))
                                  (- (list-ref b (- (length d) 1)) (* (list-ref c_ (- (- (length d) 2) 1)) (list-ref a (- (- (length d) 2) 1))))))))

  (define x (list (list-ref d_ (- (length d_) 1))))
  

  (forListX c_1 d_1 (cdr (range (- (length d) 2) -1 -1)) x (car (range (- (length d) 2) -1 -1))))


