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

(define (forListC_ a b c c_ n) ; n => length d
  (for/list ([i (range 1 (- n 1))])
    (append c_ (/ (list-ref c i) (- (list-ref b i) (* (list-ref c_ (- i 1)) (list-ref a (- i 1))))))))


(define (algorithm-thomas a b c d)
  ;a, b, c are the column vectors for the compressed tridiagonal matrix, d is the right vector
  (define c_ (/ (car c ) (car b)))
  (define d_ (/ (car d ) (car b)))

  (define c_1 (forListC_ a b c (/ (car c ) (car b)) (length d)))
  
  (define d_1 (append d_ (/ (- (list-ref d (- (length d) 1)) (* (list-ref d_ (- 
                                                                              (- (length d) 1) 1)) (list-ref d_ (- 
                                                                                                                 (- (length d) 1) 1)))) 
                            (- (list-ref b (- (length d) 1)) (* (list-ref c_ (- (- (length d) 1) 1)) (list-ref a (- (- (length d) 1) 1)))))))

  (define x (list-ref d_ (- 1)))
 
  (for/list ([i (range (- (length d) 2) -1 -1)])
    (append x (+ x (- (list-ref d_ i) (* (list-ref c_1 i) (list-ref x i)))))))


(define (teste a b)
  (define c '())
  (define d '())
  (for/list ([i (range 1 (length '(1 2 3 4 5)))])
    ;(println (append c (append a i)))
    (append d (append b (+ 1 i)))))

(teste '() '())

;(algorithm-thomas '(1 2 3) '(4 5 6) '(7 8 9) '(1 1 1 1 1 1 1 111111 11 1 1 1 1 1 11 1 11 1 2 3))

