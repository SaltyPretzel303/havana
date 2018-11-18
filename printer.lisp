#!/usr/bin/clisp

(load "creator.lisp")
(load "ascii.lisp")

(defun gen_printing_matrix(mat)
  (cond
    ((null mat) '())
    (t (mapcar (lambda(row)
                      (cons row (create_padding (calc_padding_size (car row) (length mat))))) mat))))

(defun calc_padding_size (ascii dim)
  (cond
    ((null ascii) '())
    (t (abs (- dim (1+ (get_row ascii)))))))

(defun create_padding(size)
  (cond
    ((null size) '())
    ((= 0 size))
    (t (append (list #\SPACE) (create_padding (1- size))))))

(princ (gen_printing_matrix matrix))
