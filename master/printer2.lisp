#!/usr/bin/clisp
;cera
(load "creator.lisp")

(defun gen_print_matrix (mat)
  (cond
    ((null mat) '())
    (t (setq print_mat (mapcar (lambda (row)
                                       (cond
                                         ((< (get_row (car row)) (1- (get_mat_dim matrix)))
                                          (construct_upper_half row))
                                         (t (construct_lower_half row))))
                               mat)))))

(defun construct_upper_half (row)
  (cond
    ((null row) '())
    (t (append
        (create_padding (calc_padding_size (character (car row)) (get_mat_dim matrix)))
        (list (car row))
        (list #\space)
        (extract_values (cadr row))
        ; (list #\space)
        (list (+ (get_mat_dim matrix) (get_row (car row))))
        (list #\linefeed)))))

(defun construct_lower_half (row)
  (cond
    ((null row) '())
    (t (append
        (create_padding (calc_padding_size (character (car row)) (get_mat_dim matrix)))
        (list (car row))
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
    (t (abs (- dim (1+ (get_row ascii)))))))

(defun create_padding(size)
  (cond
    ((null size) '())
    ((= 0 size) '())
    (t (append (list #\space) (create_padding (1- size))))))

(defun range(sInd eInd)
  (cond
    ((= sInd eInd) '())
    (t (append (list sInd) (list #\space) (range (1+ sInd) eInd)))))

(defun get_mat_dim (mat)
  (cond
    ((null mat) '())
    (t (1+ (floor (length mat) 2)))))

(defun add_first_row (mat)
  (setq print_mat (cons
                   (append (create_padding (1+ (calc_padding_size (code-char (1- (char-code #\A))) (get_mat_dim mat))))
                         (append (range 0 (get_mat_dim mat)) (list #\linefeed)))
                   mat)))

(defun create_p_mat (mat)
  (cond
    ((null mat) '())
    (t (add_first_row (gen_print_matrix mat)))))

(defun print_matrix()
  (mapcar (lambda (row) (mapcar (lambda (element) (princ element) ) row) ) print_mat))
