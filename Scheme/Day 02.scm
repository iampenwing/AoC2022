#lang racket

(require "AoC2022.rkt")

(define test-file "/home/penwing/Programming/AoC2022/TestInputs/Day 02.txt")
(define data-file "/home/penwing/Programming/AoC2022/Inputs/Day 02.txt")

(define rock 1)
(define paper 2)
(define scissors 3)
(define win 6)
(define draw 3)
(define loss 0)

(define (score-line-task1 score-line)
  (cond ((equal? "A X" score-line) (+ rock draw))
        ((equal? "A Y" score-line) (+ paper win))
        ((equal? "A Z" score-line) (+ scissors loss))
        ((equal? "B X" score-line) (+ rock loss))
        ((equal? "B Y" score-line) (+ paper draw))
        ((equal? "B Z" score-line) (+ scissors win))
        ((equal? "C X" score-line) (+ rock win))
        ((equal? "C Y" score-line) (+ paper loss))
        ((equal? "C Z" score-line) (+ scissors draw))))

(define (score-line-task2 score-line)
  (cond ((equal? "A X" score-line) (+ loss scissors))
        ((equal? "A Y" score-line) (+ draw rock))
        ((equal? "A Z" score-line) (+ win paper))
        ((equal? "B X" score-line) (+ loss rock))
        ((equal? "B Y" score-line) (+ draw paper))
        ((equal? "B Z" score-line) (+ win scissors))
        ((equal? "C X" score-line) (+ loss paper))
        ((equal? "C Y" score-line) (+ draw scissors))
        ((equal? "C Z" score-line) (+ win rock))))

(define task1-test
  (foldr + 0 (map score-line-task1 (read-file test-file))))
(define task1
  (foldr + 0 (map score-line-task1 (read-file data-file))))

(define task2-test
  (foldr + 0 (map score-line-task2 (read-file test-file))))
(define task2
  (foldr + 0 (map score-line-task2 (read-file data-file))))
                                  
