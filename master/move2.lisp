#!/usr/bin/clisp

(load "creator")
(load "printer2")

(defun make_move (player position)
  (cond
    ((null position) '())
    (t (cond
         ((valid_move position) (insert_move player position))
         (t (princ (append (list "Invalid move -> ") position)))))))

;position-> (row,col)
(defun insert_move (player position)
 (cond
   ((null position) '())
   (t (setf (car (cdr (assoc (car position) matrix)))
            (mapcar (lambda(element) (cond
                                       ((= (car element) (cadr position)) (list (car element) player))
                                       (t element)))
                    (car (cdr (assoc (car position) matrix))))))))

(defun valid_move (next_position)
  (cond
    ((and (>= (cadr next_position) (car (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (<= (cadr next_position) (cadr (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (< (get_row (car next_position)) (1+ (* 2 (get_mat_dim matrix))))) t)
    (t '())))

(defun move_played (position)
  (cond
    ((null position) '())
    (t ())))

(make_move #\# (list (read) (read)))
(print_matrix)
