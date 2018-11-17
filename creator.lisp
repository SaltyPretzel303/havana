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
    (t (cons (list index #\_) (gen_pairs (1+ index) end)))))

(defun gen_row (row dim)
  (cond
    ((= row (1- (* 2 dim))) '())
    (t (cons (list (get_ascii row)
                     (gen_pairs (car (get_range row dim)) (cadr (get_range row dim))))
               (gen_row (1+ row) dim)))))

(create_matrix 6)
