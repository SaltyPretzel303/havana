#!/usr/bin/clisp

(load "creator.lisp")
(load "printer.lisp")

(defun is_valid(row column)
  (cond
    ((null (get_row row)) '())
    (t (cond
         ((null (get_column column (get_row row))) '())
         (t (cond
              ((equalp (node-value (cadr (get_element row column))) #\-) t)
              (t '())))))))

(defun make_move(symbol row column)
  (cond
    ((null (is_valid row column)) '())
    (t (setf (node-value (cadr (get_element row column))) symbol))))
