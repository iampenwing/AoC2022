#lang racket

(provide read-file split-string-list sort)

; Reads a file into a list of lines
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

; Splits a list of strings into a list of lists of strings
; with a specific (discarded) string separator
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

; Sorts a list - insertion sort - ascending order
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


