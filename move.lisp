#!/usr/bin/clisp

(load "creator.lisp")
(load "printer.lisp")

(defun is_valid(row column)
  (cond
    ((null (get_row row)) '())
    (t (cond
         ((null (get_column column (get_row row))) '())
         (t (cond
              ((equalp (cadr (get_column column (get_row row))) #\-) t)
              (t '())))))))

(defun make_move(symbol row column)
  (cond
    ((null (is_valid row column)) '())
    (t (setf (cadr (get_element row column)) symbol))))

(defun make_move_input(symbol row column)
  (if (is_valid row column)
    (progn
      (setf (cadr (get_element row column)) symbol)
      (if (or (ring symbol) (fork symbol) (bridge symbol))
        t
        matrix))))

(defun make_move_sample(sample)
  (cond
    ((null sample) '())
    (t (setq matrix sample))))

; ==============================================================================

(defun next_state(prev_state symbol row column)
  (let* (
         (new_state (copy-tree prev_state))
         (changed_cell (setf (cadr (assoc column (cadr (assoc row new_state)))) symbol))
         (new_state (cons (list row column) new_state))) ; adds played coordinats before first row
    new_state))

; ==============================================================================

(defun possible_moves (prev_state symbol)
  (traverse_matrix prev_state prev_state symbol))

(defun traverse_matrix (rows prev_state symbol)
  (cond
    ((null rows) '())
    (t (append
        (traverse_row (caar rows) (cadar rows) prev_state symbol)
        (traverse_matrix (cdr rows) prev_state symbol)))))

(defun traverse_row (row_index row prev_state symbol)
  (cond
    ((null row) '())
    (t (if (equalp (cadar row) #\-)
         (cons (next_state prev_state symbol row_index (caar row)) (traverse_row row_index (cdr row) prev_state symbol))
         (traverse_row row_index (cdr row) prev_state symbol)))))

; pogledaj jel ovo okej
(defun traverse_row_new (row_index row prev_state symbol)
  (cond 
    ((null row) '())
    (t (if (equalp (cadar row) #\-)
        (cons (list row_index (caar row)) (traverse_row row_index (cdr row) prev_state symbol))
         (traverse_row row_index (cdr row) prev_state symbol)))))
; ==============================================================================

(defun compute_next_move (player)
  (let* (
         (moves (possible_moves matrix player)))
    (nth (random (length moves)) moves)))

; (princ (car (possible_moves matrix #\X)))


