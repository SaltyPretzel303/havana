#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")
(load "printer.lisp")

(setq corners (cons (list '0 '0) ; upper left
                (cons (list '0 (1- (get_mat_dim matrix))) ; upper right
                    (cons (list (1- (get_mat_dim matrix)) '0) ; middle left
                          (cons (list (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; middle right
                                (cons (list (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; lower left
                                      (cons (list (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) '())))))));lower right


;vraca T ukoliko postoji put izmedju bilo koja 2 cornera iz skupa
(defun has_bridge(ls)
  (cond ((null ls) '())
        ((equalp (length ls) 1) '())
        (t (if (null (check_bridge (list (car ls)) (cdr ls) '())) (has_bridge (cdr ls)) 't))))

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

;proverava da li su koordinate validne i ako jesu, vraca ih
(defun return_if_valid(row column)
  (if (and (and (>= row 0) (<= row (* 2 (1- (get_mat_dim matrix)))))
       (and (>= column (car (get_range row (get_mat_dim matrix)))) (< column (cadr (get_range row (get_mat_dim matrix))))))
    (list (list row column))))

;nalazi susede start-a sa istim znakom
(defun add_neighbours (start visited)
  (new_nodes (fields_with_symbol (cadr (get_element (car start) (cadr start))) (get_neighbours (car start) (cadr start))) visited))

;vraca listu suseda koji vec nisu poseceni
(defun new_nodes (neighbours visited)
  (cond ((null neighbours) '())
        ((member (car neighbours) visited :test 'equalp)
          (new_nodes (cdr neighbours) visited))
        (t (cons (car neighbours) (new_nodes (cdr neighbours) visited)))))

;ako se neki element prve liste, nalazi u drugoj => T else '()
(defun compare_lists(startList endList)
  (cond ((null startList) '())
        (t (if (member (car startList) endList :test 'equalp) (car startList) (compare_lists (cdr startList) endList)))))

;vratice T ako postoji put od start polja do nekog od end polja 
(defun check_bridge (start end visited)
  (cond 
    ((compare_lists start end) 't)
    ((null start) '())
    (t (let* ((visited1 (cons (car start) visited))
              (neighbours (add_neighbours (car start) (append start visited1)))
              (start1 (append (cdr start) neighbours))
              (is_connected (check_bridge start1 end visited1)))
            is_connected))))


;TESTING

(make_move 'X '0 '0) ; 0 0
(make_move 'X  '0 (1- (get_mat_dim matrix))) ; 0 5
(make_move 'O (1- (get_mat_dim matrix)) '0) ; 5 0
(make_move 'X (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; 5 10
(make_move 'X (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; 10 5
(make_move 'O (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) ; 10 10
(make_move 'X '1 '1)
(make_move 'X '2 '3)
(make_move 'X '3 '3)
(make_move 'X '2 '2)
(make_move 'X '4 '3)
(make_move 'X '4 '4)
;(make_move 'X '5 '4)
(make_move 'X '6 '4)
(make_move 'X '7 '5)
(make_move 'X '7 '6)
(make_move 'X '7 '7)
(make_move 'X '8 '5)
(make_move 'X '6 '7)
(make_move 'X '5 '7)
(make_move 'X '5 '8)
(make_move 'X '5 '9)
(make_move 'X '9 '5)
(print_matrix)
;(trace add_neighbours)
;(untrace get_element)
;(trace check_bridge)
;(princ (check_bridge '((0 0)) '((5 10) (10 5)) '()))

;(princ corners)
;(princ #\linefeed)
;(princ (fields_with_symbol 'X corners))
;(trace check_bridge)
;(has_bridge (fields_with_symbol 'X corners))


; (follow_path_from_to '() '0 '0 '(('10 5)) 'X)

; (mark_as_visited '0 '0)
; (princ (append (get_element '0 '0) (list #\#)))
;(princ matrix)

; (mark_as_visited '0 '0)

;
; (princ #\linefeed)
; (princ corners)
; (princ #\linefeed)
; (princ (fields_with_symbol 'X corners))
