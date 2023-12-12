#lang racket

;; Calculate the number of grains of wheat on a chessboard given that the number on each square doubles.

;; There once was a wise servant who saved the life of a prince.
;; The king promised to pay whatever the servant could dream up.
;; Knowing that the king loved chess, the servant told the king he would like to have grains of wheat.
;; One grain on the first square of a chess board, with the number of grains doubling on each successive square.
;; There are 64 squares on a chessboard (where square 1 has one grain, square 2 has two grains, and so on).

;; Write code that shows:
;; 1) how many grains were on a given square, and
;; 2) the total number of grains on the chessboard

(module+ test
  (require rackunit))

(require racket/trace)

(module+ test
  ;; Test code that is run with `raco test`
  ;; NOTE: does not run when this file is required by another module
  (check-equal? (+ 2 2) 4)
  (check-equal? (fact 0) 0)
  (check-equal? (fact 1) 1)
  (check-equal? (fact 2) 2)
  (check-equal? (fact 3) 6)
  (check-equal? (fact 4) 24)
  (check-equal? (fact 5) 120)
  (check-equal? (factT 0) 0)
  (check-equal? (factT 1) 1)
  (check-equal? (factT 2) 2)
  (check-equal? (factT 3) 6)
  (check-equal? (factT 4) 24)
  (check-equal? (factT 5) 120)
  (check-equal? (total-squares-to 0) 0)
  (check-equal? (total-squares-to 1) 1)
  (check-equal? (total-squares-to 2) 3)
  (check-equal? (total-squares-to 3) 7)
  (check-equal? (total-squares-to 4) 15)
  (check-equal? (total-squares-to 5) 31))

#|
Instructions
Find the difference between the square of the sum and the sum of the squares of the first N natural numbers.

The square of the sum of the first ten natural numbers is (1 + 2 + ... + 10)² = 55² = 3025.

The sum of the squares of the first ten natural numbers is 1² + 2² + ... + 10² = 385.

Hence the difference between the square of the sum of the first ten natural numbers and the sum of the squares of the first ten natural numbers is 3025 - 385 = 2640.

You are not expected to discover an efficient solution to this yourself from first principles; research is allowed, indeed, encouraged. Finding the best algorithm for the problem is a key skill in software engineering.

#lang racket
(provide sum-of-squares square-of-sum difference)
(define (sum-of-squares number)
  (/ (* number (+ number 1) (+ (* number 2) 1)) 6))
(define (square-of-sum number)
  (expt (sum-of-naturals number) 2))
(define (difference number)
  (- (square-of-sum number) (sum-of-squares number)))
(define (sum-of-naturals N)
  (/ (* N (+ N 1)) 2))


|#





(define (fact N)
  ;; calculate factorial using embeded recursion
  (cond
    [(<= N 0) 0]
    [(= N 1) 1]
    [else (* N (fact (- N 1)))]))

(define (factT N)
  (factT-helper N 1))

(define (factT-helper Z answer-so-far)
  (cond
    [(<= Z 0) 0]
    [(= Z 1) answer-so-far]
    [else (factT-helper (- Z 1) (* Z answer-so-far))]))

(define (calc-square N)
  (if (< N 1) 0 (expt 2 (- N 1))))

(define (total-squares-to N)
  (total-squaresT N 1))

(define (total-squaresT Z total-so-far)
  (cond
    [(< Z 1)
     (set! total-so-far 0)
     total-so-far]
    [(= Z 1) total-so-far]
    [else (total-squaresT (- Z 1) (+ (calc-square Z) total-so-far))]))

;;(provide hello)

;;(define (hello)
;;  (displayln "Hello, World!"))

;;(hello)
(define (calc-base-two n)
  (expt 2 n))

;;(define (sum-to n)
;;  (for/sum ([i (in-range 1 (add1 n))])
;;    i))

(define (sum-squares-to n)
  (for/sum ([i (in-range 1 (add1 n))]) (* i i)))

(trace fact)
(trace factT-helper)
(trace total-squaresT)

(module+ main
  ;; main submodule
  ;; NOTE: does not run when this file is required by another module
  (define exponent 30)
  (displayln "This Racket program solves the chessboard Exercism problem.")
  (displayln (format "What is the 2^~a?" exponent))
  (displayln (format "Answer:~a" (calc-base-two exponent)))
  (sum-squares-to 5)
  (total-squares-to 64))
