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
    (t (progn
        (setq matrix sample)
        (if (or (ring symbol) (fork symbol) (bridge symbol))
          t
          matrix)))))

(defun next_state(prev_state symbol row column)
  (let* (
         (new_state prev_state)
         (changed_cell (setf (cadr (assoc column (cadr (assoc row new_state)))) symbol)))
    new_state))

; (princ (next_state matrix #\X '0 '0))
; (princ #\linefeed)
; (princ matrix)
