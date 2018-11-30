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

(defun fields_with_symbol (symbol ls)
  (cond
    ((null ls) '())
    (t (if (equalp (cadr (get_element (car (car ls))(cadr (car ls)))) symbol)
          (cons (car ls) (fields_with_symbol symbol (cdr ls)))
         (fields_with_symbol symbol (cdr ls))))))

(defun get_neighbours(row column)
  (if (or (null row) (null column))
    (princ (list "ne radi za1: " row column)); invalid coordinats
    (append (return_if_valid (1- row) (1- column)) ; upper left
          (append (return_if_valid (1- row) column) ; upper right
                (append (return_if_valid row (1- column)) ; left
                      (append (return_if_valid row (1+ column)) ; right
                            (append (return_if_valid (1+ row) column) ; lower left
                                  (append (return_if_valid (1+ row) (1+ column)) '())))))))) ; lower riht

(defun return_if_valid(row column)
  (if (and (and (>= row 0) (<= row (* 2 (1- (get_mat_dim matrix)))))
       (and (>= column (car (get_range row (get_mat_dim matrix)))) (< column (cadr (get_range row (get_mat_dim matrix))))))
    (list (list row column))))

; (defun follow_path_from_to(visited row column goals symbol)
;   (cond
;     ((null goals) '())
;     (t (if (member (list row column) goals)
;          (t) ; true -> some goal reached
;          (mapcar (lambda(neighbour)
;                         (progn
;                          (princ (list row column #\linefeed))
;                          (princ visited)
;                          (follow_path_from_to (cons (list row column) visited) (car neighbour) (cadr neighbour) goals symbol)))
;                  (set-difference (fields_with_symbol symbol (get_neighbours row column)) visited))))))

(defun follow_path_from_to(visited row column goals symbol)
  (cond
    ((null goals) '())
    (t (if (member (list row column) goals)
         (t) ; true -> some goal reached
         (follow_neighbours visited row column goals symbol (set-difference (fields_with_symbol symbol (get_neighbours row column)) visited))))))

(defun follow_neighbours(visited row column goals symbol neighbours)
  (cond
    ((null neighbours) '())
    (t (progn
        (princ (list row column))
        (follow_path_from_to (cons (list row column) visited) (caar neighbours) (cadar neighbours) goals symbol)
        (follow_neighbours (cons (list row column) visited) (caar neighbours) (cadar neighbours) goals (cdr neighbours))))))

(defun get_side_coordinats()
  (setq sides (cons ())))

(defun get_horizontal_sides(index)
  ())

(defun get_vertical_sides()
  ())

(make_move 'X '0 '0) ; 0 0
(make_move 'X  '0 (1- (get_mat_dim matrix))) ; 0 5
(make_move 'O (1- (get_mat_dim matrix)) '0) ; 5 0
(make_move 'X (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; 5 10
(make_move 'X (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; 10 5
(make_move 'O (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) ; 10 10
(make_move 'X '1 '1)
; (make_move 'X '2 '3)
(make_move 'X '3 '3)
(make_move 'X '2 '2)
(make_move 'X '4 '3)
; (make_move 'X '4 '4)
(make_move 'X '5 '4)
(make_move 'X '6 '4)
(make_move 'X '7 '5)
(print_matrix)

(follow_path_from_to '() '0 '0 '(('10 5)) 'X)

;
; (princ #\linefeed)
; (princ corners)
; (princ #\linefeed)
; (princ (fields_with_symbol 'X corners))
