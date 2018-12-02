#!/usr/bin/clisp

(load "ascii.lisp")

(defun create_matrix (dim)
  (cond
    ((< dim 0) '())
    (t (setq matrix (gen_row 0 dim)))))

(defun get_range(row dim)
  (cond
    ((< row 0) '(0 0))
    (t (cond
         ((< row dim) (list 0 (+ dim row)))
         (t (list (1+ (- row dim)) (- (* 2 dim) 1)))))))

(defun gen_pairs (index end)
  (cond
    ((= index end) '())
    (t (cons (list index  #\-) (gen_pairs (1+ index) end)))))

(defun gen_row (row dim)
  (cond
    ((= row (1- (* 2 dim))) '())
    (t (cons (list row
                   (gen_pairs (car (get_range row dim)) (cadr (get_range row dim))))
             (gen_row (1+ row) dim)))))

; functions for matrix generation ^^^^

; functions helpers

(defun get_mat_dim (mat)
  (cond
    ((null mat) '())
    (t (1+ (floor (length mat) 2)))))

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

(defun mark_as_visited(row column)
  (setf (assoc column (assoc row matrix)) (append (get_element row column) (list #\v))))

(defun is_visited (row column)
  (if (equalp (car (cddr (get_element row column))) #\v) ; get_elemnt returns-> (1 X 'v') -> 'v' if visited
    (t)
    '())) ; field is visited (marked with 'v' on third place)

(create_matrix 6)
