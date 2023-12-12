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
#lang racket

(provide two-fer)

(define (two-fer [name "you"])
  ;; check if the function returns the proper values for two-fer problem
  ;; 1) Is a string?
  ;; 2) Is an empty string?
  ;; 3) Is an all whitespace string?
  ;; 4) Else, return the interpolated version
  (if (string-empty? (string-trim name))
      "One for you, one for me."
      (format "One for ~a, one for me." name)))

(define (string-empty? STR)
  (= (string-length STR) 0))
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
