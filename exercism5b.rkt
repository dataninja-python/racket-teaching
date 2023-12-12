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

#|

  Instructions

  For a game of Dungeons & Dragons, each player starts by generating a character they can play with. This character has, among other things, six abilities; strength, dexterity, constitution, intelligence, wisdom and charisma. These six abilities have scores that are determined randomly. You do this by rolling four 6-sided dice and record the sum of the largest three dice. You do this six times, once for each ability.

  Your character's initial hitpoints are 10 + your character's constitution modifier. You find your character's constitution modifier by subtracting 10 from your character's constitution, divide by 2 and round down.

  Write a random character generator that follows the rules above.

  For example, the six throws of four dice may look like:

  5, 3, 1, 6: You discard the 1 and sum 5 + 3 + 6 = 14, which you assign to strength.
  3, 2, 5, 3: You discard the 2 and sum 3 + 5 + 3 = 11, which you assign to dexterity.
  1, 1, 1, 1: You discard the 1 and sum 1 + 1 + 1 = 3, which you assign to constitution.
  2, 1, 6, 6: You discard the 1 and sum 2 + 6 + 6 = 14, which you assign to intelligence.
  3, 5, 3, 4: You discard the 3 and sum 5 + 3 + 4 = 12, which you assign to wisdom.
  6, 6, 6, 6: You discard the 6 and sum 6 + 6 + 6 = 18, which you assign to charisma.
  Because constitution is 3, the constitution modifier is -4 and the hitpoints are 6.

  Notes:
  Most programming languages feature (pseudo-)random generators, but few programming languages are designed to roll dice. One such language is Troll.

|#

(define (roll-6d)
  (random 1 7))

(define (roll-n-times rolls)
  (roll-n-times-helperT rolls '()))

(define (roll-n-times-helperT N current-list-of-rolls)
  (if (< N 1)
      current-list-of-rolls
      (roll-n-times-helperT (- N 1) (cons (roll-6d) current-list-of-rolls))))

;; quick-sort-L
;; checks:

;; quick-sort-L
;; purpose: return the largest element (a number) from a list (L)
;; inputs: list (L)
;; outputs: element (E)
;; requirements:
;; constraints:
;; description: uses tail recursion
;; notes:
(define (quick-sort-L L)
  ;; uses quick sort algorithm on a list
  (if (or (empty? L) (empty? (rest L)))
      L
      (let* ([pivot (first L)]
             [rest (rest L)]
             [less (filter (lambda (X) (< X pivot)) rest)]
             [greater (filter (lambda (X) (>= X pivot)) rest)])
        (append (quick-sort-L less) (list pivot) (quick-sort-L greater)))))

(define (get-largest-list L)
  (rest (quick-sort-L L)))

(define (sum-L L)
  (sum-L-helper L 0))

(define (sum-L-helper L value-so-far)
  (cond
    [(null? L) 0]
    [(null? (rest L)) (+ (first L) value-so-far)]
    [else (sum-L-helper (rest L) (+ (first L) value-so-far))]))

(module+ main
  ;; main submodule
  ;; NOTE: does not run when this file is required by another module
  (define number-of-rolls 4)
  (displayln (format "Roll dice: ~a" number-of-rolls))
  (define rolls-list (roll-n-times number-of-rolls))
  (displayln rolls-list)
  (displayln (quick-sort-L rolls-list))
  (set! rolls-list (get-largest-list rolls-list))
  (displayln (format "~a" (sum-L rolls-list))))
