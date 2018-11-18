#!/usr/bin/clisp
;dacha
(load "creator")

(setq n 6)

(defun calculate_padding (dim letter)
  (abs (- dim (+ 1 (get_row letter)))))

(defun create_padding(dim letter counter)
  (if (< counter (calculate_padding dim letter)) (append (list #\SPACE) (create_padding dim letter (+ 1 counter)))
    '()))

(defun extract_values(values)
  (cond
   ((null values) '())
   (t (append (list (car(cdr(car values))))
              (list #\SPACE)
              (extract_values (cdr values))))))

(defun prepare_row (row)
  (cond
   ((null row) '())
   (t (append (list (car row)) (create_padding n (car row) '0) (extract_values (cadr row))))))

(defun prepare_matrix(mat)
  (cond
   ((null mat) '())
   (t (append (prepare_row (car mat)) (list #\linefeed)
              (prepare_matrix (cdr mat))))))

; (setq print_mat (cons
;                  (append (create_padding (calc_padding_size (code-char (1- (char-code #\A))) (1+ (floor (length print_mat) 2))))
;                        (range 0 (1+ (floor (length print_mat) 2))))
;                  print_mat))

(princ (prepare_matrix matrix))

; (mapcar (lambda (row) (princ row) ) (prepare_matrix matrix))
