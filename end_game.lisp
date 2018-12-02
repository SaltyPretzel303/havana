#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")
(load "printer.lisp")
(load "game.lisp")

; in variable corners sets coordinats of all corners
(setq corners (cons (list '0 '0) ; upper left
                (cons (list '0 (1- (get_mat_dim matrix))) ; upper right
                    (cons (list (1- (get_mat_dim matrix)) '0) ; middle left
                          (cons (list (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) ; middle right
                                (cons (list (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix))) ; lower left
                                      (cons (list (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) '())))))));lower right

(defun row_coordinates(index row)
  (cond
    ((null row) '())
    (t (progn
         (cons (list index (caar row)) (row_coordinates index (cdr row)))))))

(defun side_coordinates (row_index)
  (if (< row_index (* 2 (1- (get_mat_dim matrix))))
    (let* (
            (row (cadr (get_row row_index)))
            (first_index (car (first row)))
            (last_index (car (car (last row))))) ; last returns '(last_element) -> (car (last list)) = last_element
          (append (list (list row_index first_index) (list row_index last_index)) (side_coordinates (1+ row_index))))
    '()))

(setf sides (let (
                  (top (row_coordinates 0 (cadr (get_row 0))))
                  (bottom (row_coordinates (* 2 (1- (get_mat_dim matrix ))) (cadr (get_row (* 2 (1- (get_mat_dim matrix)))))))
                  (left_right (side_coordinates 1)))
              (append top left_right bottom)))



(defun find_bridge(symbol)
  (let* (
         (played_corners (fields_with_symbol symbol corners))
         (start_point (car played_corners))
         (goals (cdr played_corners))) ; exclude starting point from goals
    (cond
      ((null (follow_path_from_to (car start_point) (cadr start_point) goals symbol '0))
       (progn
         (princ "can't find bridge do something...")))
      (t (progn
          (princ "bridge found...."))))))



(defun find_fork(symbol row column)
  ())

(defun fields_with_symbol (symbol ls)
  (cond
    ((null ls) '())
    (t (if (equalp (node-value (cadr (get_element (car (car ls))(cadr (car ls))))) symbol)
          (cons (car ls) (fields_with_symbol symbol (cdr ls)))
         (fields_with_symbol symbol (cdr ls))))))

(defun symbol_and_neutral (symbol ls)
  (cond
    ((null ls) '())
    (t (if (not (equalp (node-value (cadr (get_element (car(car ls)) (cadr (car ls))))) (next_player symbol)))
         (cons (car ls) (symbol_and_neutral symbol (cdr ls)))
         (symbol_and_neutral symbol (cdr ls))))))

(defun neutral_from (ls)
  (cond
    ((null ls) '())
    (t (if (equalp (node-value (cadr (get_element (car (car ls))(cadr (car ls))))) #\-)
          (cons (car ls) (neutral_from (cdr ls)))
         (neutral_from (cdr ls))))))

(defun get_neighbours(row column)
  (if (or (null row) (null column))
    (princ (list "invalid coordinat in get_neighbours : " row column)); invalid coordinats
    (append (return_if_valid (1- row) (1- column)) ; upper left
          (append (return_if_valid (1- row) column) ; upper right
                (append (return_if_valid row (1- column)) ; left
                      (append (return_if_valid row (1+ column)) ; right
                            (append (return_if_valid (1+ row) column) ; lower left
                                  (append (return_if_valid (1+ row) (1+ column)) '())))))))) ; lower riht

; may be duplicate of move -> is_valid
(defun return_if_valid(row column)
  (if (and (and (>= row 0) (<= row (* 2 (1- (get_mat_dim matrix)))))
       (and (>= column (car (get_range row (get_mat_dim matrix)))) (< column (cadr (get_range row (get_mat_dim matrix))))))
    (list (list row column))))

; removes visited fields from given list
(defun not_visited_from (ls)
  (cond ((null ls) '())
    (t (if (not (is_visited (caar ls) (cadar ls)))
          (cons (car ls) (not_visited_from (cdr ls)))
          (not_visited_from (cdr ls))))))

; removes visited or selected fields from given list
(defun not_visited_or_selected_from (ls)
  (cond ((null ls) '())
    (t (progn
            (if (not (or (is_visited (caar ls) (cadar ls))
                         (is_selected (caar ls) (cadar ls))))
              (cons (car ls) (not_visited_or_selected_from (cdr ls))) ; not visited or selected for visiting
              (not_visited_or_selected_from (cdr ls))))))) ; visited or selected for visiting

(defun follow_path_from_to(row column goals symbol depth)
  (cond
    ((null goals) '())
    (t (if (member (list row column) goals :test 'equal)
         (progn
          (princ (list "REACHED GOAL : " row column)) ; debug message
          (reset_visited_nodes visited_nodes) ; reseting visited nodes
          t)
         (progn
          (princ (list "visiting: " row column #\linefeed)) ; debug message
          (mark_as_visited row column) ; marks temp node as visited
          (let* (
                  (ret_val (follow_neighbours goals symbol (mark_list_as_selected (not_visited_or_selected_from (fields_with_symbol symbol (get_neighbours row column)))) depth))
                  (if_value (if (equal depth '0)
                              (reset_visited_nodes visited_nodes)))) ; if all recursions are done (back to inital call (depth=0)) reset all visited nodes
            ret_val)))))) ; return value of follow_path_from_to recursive call

          ;(mark_list_as_selected (not_visited_or_selected_from (fields_with_symbol symbol (get_neighbours row column))))
          ; gets all neighbours with same symbol, removes visited or selected for visiting, selecte all of them as selected for visiting and sends them as neighbours to
          ; follow_neighbours function

; just calls follow_path_from_to for all given neighbours
(defun follow_neighbours(goals symbol neighbours depth)
  (cond
    ((null neighbours) '())
    (t (if (null (follow_path_from_to (caar neighbours) (cadar neighbours) goals symbol (1+ depth))) ; follow next element in path (first from neigbours) if goal is not reached
        (progn
         (setq visited_nodes (remove_node_from (car neighbours) visited_nodes))
         (follow_neighbours goals symbol (cdr neighbours) depth))                                 ; t is returned in case of reaching goal
        t))))

; ==============================================================================

(defun ring (symbol row column)
  (find_ring symbol (neutral_from (get_neighbours row column))))

(defun find_ring(symbol potential)
  (cond
    ((null potential)
     (progn
      (reset_visited_nodes visited_nodes)
      '()))
    (t (if (check_ring_from symbol (caar potential) (cadar potential))
          (progn
           (reset_visited_nodes visited_nodes)
            t)
          (progn
            (princ (list "not visited neutral: " (not_visited_from (cdr potential))))
            (find_ring symbol (not_visited_from (cdr potential))))))))

(defun check_ring_from(symbol row column)
  (if (equal (node-value (cadr (get_element row column))) symbol)
        (progn
         (princ (list "found " symbol " on: " row column #\linefeed))
         (mark_as_visited row column)
          t)
        (if (is_on_side row column)
              (progn
               (princ (list "got on side on: " row column #\linefeed))
               (mark_as_visited row column)
               '())
              (progn
                (princ (list "visiting neighbours from: " row column))
                (mark_as_visited row column)
                (check_surrounding symbol (symbol_and_neutral symbol (not_visited_from (get_neighbours row column))) '())))))

(defun check_surrounding (symbol neighbours last)
  (cond
    ((null neighbours) last)
    (t (if (not (null (check_ring_from symbol (caar neighbours) (cadar neighbours))))
         (check_surrounding symbol (not_visited_from (cdr neighbours)) t)
         '()))))

(defun is_on_side(row column)
  (member (list row column) sides :test 'equal))
; ==============================================================================

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
(make_move 'X '8 '5)

(make_move 'X '3 '4)
(make_move 'X '3 '5)
(make_move 'X '4 '6)
(make_move 'X '5 '6)
(make_move 'X '6 '6)
(make_move 'X '7 '6)
(make_move 'X '3 '6)
(make_move 'X '9 '5)

(print_matrix)

(princ (ring 'X '6 '4))



; (find_bridge 'X)
; (princ visited_nodes)


; (follow_path_from_to '0 '0 '( (0 5) (5 10) (10 5)) 'X)

; (princ (list "visited" visited_nodes))

; (setq a '((1 2) (2 3 ) (3 4)))
; (princ (remove (list 1 2) a :test 'equal))
