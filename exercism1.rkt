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

(module+ test
  ;; Test code that is run with `raco test`
  ;; NOTE: does not run when this file is required by another module
  (check-equal? (+ 2 2) 4))

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
  (for/sum ([i (in-range 1 (add1 n))])
    (* i i)))

(module+ main
  ;; main submodule
  ;; NOTE: does not run when this file is required by another module
  (define exponent 30)
  (displayln "This Racket program solves the chessboard Exercism problem.")
  (displayln (format "What is the 2^~a?" exponent))
  (displayln (format "Answer:~a" (calc-base-two exponent)))
  (sum-squares-to 5))
