#!/usr/bin/clisp
;cera
(load "creator.lisp")

(defun gen_print_matrix (mat)
  (cond
    ((null mat) '())
    (t (setq print_mat (mapcar (lambda (row)
                                       (cond
                                         ((< (car row) (1- (get_mat_dim matrix)))
                                          (construct_upper_half row))
                                         (t (construct_lower_half row))))
                               mat)))))

(defun construct_upper_half (row)
  (cond
    ((null row) '())
    (t (append
        (create_padding (calc_padding_size (car row) (get_mat_dim matrix)))
        ; (list (get_ascii (car row)))
        (list (car row)) ; for debug
        (list #\space)
        (extract_values (cadr row))
        ; (list #\space)
        (list (+ (get_mat_dim matrix) (car row)))
        (list #\linefeed)))))

(defun construct_lower_half (row)
  (cond
    ((null row) '())
    (t (append
        (create_padding (calc_padding_size (car row) (get_mat_dim matrix)))
        ; (list (get_ascii (car row)))
        (list (car row)) ; for debug
        (list #\space)
        (extract_values (cadr row))
        (list #\linefeed)))))

(defun extract_values (row)
  (cond
    ((null row )'())
    (t (append (list (cadar row)) (list #\space) (extract_values (cdr row))))))

(defun calc_padding_size (ascii dim)
  (cond
    ((null ascii) 0)
    (t (abs (- dim (1+ ascii))))))

(defun create_padding(size)
  (cond
    ((null size) '())
    ((= 0 size) '())
    (t (append (list #\space) (create_padding (1- size))))))

(defun range(sInd eInd)
  (cond
    ((= sInd eInd) '())
    (t (append (list sInd) (list #\space) (range (1+ sInd) eInd)))))

(defun add_first_row (mat)
  (setq print_mat (cons
                   (append (create_padding (1+ (calc_padding_size -1 (get_mat_dim mat))))
                         (append (range 0 (get_mat_dim mat)) (list #\linefeed)))
                   mat)))

(defun print_matrix()
  (mapcar (lambda (row)
                  (mapcar (lambda (element) (princ element) ) row))
          (add_first_row (gen_print_matrix matrix))))

; (print_matrix)
