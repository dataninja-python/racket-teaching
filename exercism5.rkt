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

;; get the largest element from a list of numbers
;; checks:
;; 1) (null? L) = '()
;; 2) (null? (rest L)) = L
;; 3) (= (length L) 2) = (cons (min (first L) (rest L)) (max (first L) (rest L)))
;; 4) else return list with the largest item at end of list

;; get-max-element
;; purpose: return the largest element (a number) from a list (L)
;; inputs: list (L)
;; outputs: element (E)
;; requirements:
;; constraints:
;; description: uses tail recursion
;; notes:
(define (get-max-element L)
  (get-max-element-helperT L))

;; get-largest-max-helperT
(define (get-max-element-helperT L [max-element 0] [element 0])
  (if (null? L) 0 (get-max-element-helperT (rest L) (set! element (first L)))))

(define (last L)
  (last-helperT L 0))

(define (last-helperT L E)
  (if (null? (rest L)) E (last-helperT (rest L) E)))

(define (sort-list L)
  (sort-list-helperT L '()))

(define (sort-list-helperT L NL)
  (cond
    ;; if L = empty, return empty
    [(null? L) '()]
    ;; if L has 1 element, return this as the list
    [(null? (last L))
     (set! NL L)
     NL]
    [else (sort-list-helperT)]))

(module+ main
  ;; main submodule
  ;; NOTE: does not run when this file is required by another module
  (define number-of-rolls 4)
  (displayln (format "Roll dice ~a" number-of-rolls))
  (define rolls-list (roll-n-times number-of-rolls))
  (displayln rolls-list)
  (get-max-element rolls-list))
