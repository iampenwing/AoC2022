#lang racket

(require "AoC2022.rkt")

(define test-file "/home/penwing/Programming/AoC2022/TestInputs/Day 01.txt")
(define data-file "/home/penwing/Programming/AoC2022/Inputs/Day 01.txt")

(define task1-test (apply max (map (lambda (x) (foldr + 0 x)) (map (lambda (x) (map string->number x)) (split-string-list "" (read-file test-file))))))
(define task1 (apply max (map (lambda (x) (foldr + 0 x)) (map (lambda (x) (map string->number x)) (split-string-list "" (read-file data-file))))))

(define task2-test (let ((data-list (reverse (sort (map (lambda (x) (foldr + 0 x))  (map (lambda (x) (map string->number x)) (split-string-list "" (read-file test-file))))))))
                     (+ (car data-list) (car (cdr data-list)) (car (cdr (cdr data-list))))))
(define task2 (let ((data-list (reverse (sort (map (lambda (x) (foldr + 0 x))  (map (lambda (x) (map string->number x)) (split-string-list "" (read-file data-file))))))))
                     (+ (car data-list) (car (cdr data-list)) (car (cdr (cdr data-list))))))
