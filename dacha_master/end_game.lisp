#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")
(load "printer.lisp")

(defun find_bridge(symbol row column)
  ())

(defun find_fork(symbol row column)
  ())

; in variable corners sets coordinats of all corners
(setq corners (cons (list '0 '0) ; upper left
                (cons (list '0 (1- (get_mat_dim matrix))) ; upper right
                    (cons (list (1- (get_mat_dim matrix)) '0) ; middle left
                          (cons (list (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; middle right
                                (cons (list (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; lower left
                                      (cons (list (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) '())))))));lower right

(defun remove_clear_corners (symbol ls)
  (cond
    ((null ls) '())
    (t (if (equalp (cadr (get_element (car (car ls))(cadr (car ls)))) symbol)
          (cons (car ls) (remove_clear_corners symbol (cdr ls)))
         (remove_clear_corners symbol (cdr ls))))))

(make_move 'O '0 '0) ; 0 0
(make_move 'X  '0 (1- (get_mat_dim matrix))) ; 0 5
(make_move 'O (1- (get_mat_dim matrix)) '0) ; 5 0
(make_move 'X (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; 5 10
(make_move 'X (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; 10 5
(make_move 'O (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) ; 10 10
(print_matrix)

(princ #\linefeed)
(princ corners)
(princ #\linefeed)
(princ (remove_clear_corners 'X corners))