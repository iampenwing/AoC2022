#lang racket
(define test-file "/home/penwing/Programming/AoC2022/Inputs/TestInput 01.txt")
(define data-file "/home/penwing/Programming/AoC2022/Inputs/Input 01.txt")

(define (read-file file-name)
  (let ((file-port (open-input-file file-name)))
    (let ((file-as-list (read-file-helper file-port)))
      (close-input-port file-port)
      file-as-list)))

(define (read-file-helper inport)
  (let ((line (read-line inport)))
    (if (eof-object? line)
	'()
	(cons line (read-file-helper inport)))))

(define (split-string-list split-at string-list)
  (if (empty? string-list)
      '()
      (let-values ([(top-list rest-of-list) (split-string-list-helper '() split-at string-list)])
        (cons top-list (split-string-list split-at rest-of-list)))))

(define (split-string-list-helper current-list split-at string-list)
  (if (empty? string-list)
      (values current-list string-list)
      (if (equal? split-at (car string-list)) 
          (values current-list (cdr string-list))
          (split-string-list-helper (append current-list (list (car string-list))) split-at (cdr string-list)))))

(define (sort unordered-list)
  (if (empty? unordered-list)
      '()
      (insert-element (car unordered-list) (sort (cdr unordered-list)))))

(define (insert-element x xs)
  (if (empty? xs)
      (cons x '())
      (if (< x (car xs))
          (cons x xs)
          (cons (car xs) (insert-element x (cdr xs))))))


(define task1-test (apply max (map (lambda (x) (foldr + 0 x)) (map (lambda (x) (map string->number x)) (split-string-list "" (read-file test-file))))))
(define task1 (apply max (map (lambda (x) (foldr + 0 x)) (map (lambda (x) (map string->number x)) (split-string-list "" (read-file data-file))))))

(define task2-test (let ((data-list (reverse (sort (map (lambda (x) (foldr + 0 x))  (map (lambda (x) (map string->number x)) (split-string-list "" (read-file test-file))))))))
                     (+ (car data-list) (car (cdr data-list)) (car (cdr (cdr data-list))))))
(define task2 (let ((data-list (reverse (sort (map (lambda (x) (foldr + 0 x))  (map (lambda (x) (map string->number x)) (split-string-list "" (read-file data-file))))))))
                     (+ (car data-list) (car (cdr data-list)) (car (cdr (cdr data-list))))))
