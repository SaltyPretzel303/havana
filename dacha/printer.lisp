#!/usr/bin/clisp

(load "prva_faza.lisp")

(setq n 6)

(defun calculate_padding (dim letter)
  (abs (- dim (+ 1 (- (char-code letter) 65)))))

(defun print_padding(dim letter counter)
  (if(< counter (calculate_padding dim letter)) (append (list #\SPACE) (print_padding dim letter (+ 1 counter)))
    '()))

(defun extract_values(values)
  (cond
   ((null values) '())
   (t (append (list (car(cdr(car values)))) (list #\SPACE) (extract_values (cdr values))))))

(defun prepare_row (row)
  (cond
   ((null row) '())
   (t (append (list (car row)) (print_padding n (car row) '0) (extract_values (cadr row))))))

(defun prepare_matrix(mat)
  (cond
   ((null mat) '())
   (t (append (prepare_row (car mat)) (list #\linefeed) (prepare_matrix (cdr mat))))))

(mapcar (lambda (row) (princ row) ) (prepare_matrix mat))
