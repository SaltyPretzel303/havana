#!/usr/bin/clisp
(setq n 6)

(defun generate_ascii_list(num dim)
  (cond
   ((zerop dim) '())
   (t (cons (code-char num) (generate_ascii_list (+ num 1) (- dim 1))))))

(defun calculate_ending_index (dim counter)
  (if (< counter dim) (+ counter dim) (- (* dim 2) 1)))

(defun calculate_starting_index (dim counter)
  (if (< counter dim) '0 (+ 1 (- counter dim))))

(defun init_row(dim letterCounter counter)
  (cond ((>= counter (calculate_ending_index dim letterCounter)) '())
        (t (cons (list counter '-) (init_row dim letterCounter (+ 1 counter))))))

(defun init_matrix2 (dim list letterCounter)
  (cond
   ((null list) '())
   (t (cons (list (car list) (init_row dim letterCounter (calculate_starting_index dim letterCounter)))
            (init_matrix2 dim (cdr list) (+ letterCounter 1))))))

(defun generate_matrix(dim)
  (cond
    ((null dim) '())
    ((< dim 0) '())
    (t (setq mat (init_matrix2 dim (generate_ascii_list 65 (- (* dim 2) 1)) '0)))))

(generate_matrix 6)
