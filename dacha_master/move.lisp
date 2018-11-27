#!/usr/bin/clisp

(load "creator.lisp")
(load "printer.lisp")

(defun get_row (row)
  (cond
    ((null row) '())
    (t (assoc row matrix))))

(defun get_column(index row)
  (cond
    ((null row) '())
    (t (assoc index (cadr row)))))

(defun get_element(row column)
  (get_column column (get_row row)))

(defun is_valid(row column)
  (cond
    ((null (get_row row)) '())
    (t (cond
         ((null (get_column column (get_row row))) '())
         (t (cond
              ((equalp (cadr (get_column column (get_row row))) #\-) t)
              (t '())))))))

(defun make_move(symbol row column)
  (cond
    ((null (is_valid row column)) '())
    (t (setf (cadr (get_element row column)) symbol))))
