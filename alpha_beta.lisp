#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

; returns (evaluation (row, column))
(defun alpha_beta(board alpha beta depth player) ; rule -> min || max
  (cond
    ((or (= depth 0) (terminal_node board)) (evaluate_board temp_board)) ; terminal_node -> no more valid moves
    (t (cond
        ((equalp player #\X)
            (get_max_move (possible_moves board player) alpha beta depth player))
        ((equalp player #\O)
            (get_min_move (possible_moves board player) alpha beta depth player))))))

(defun get_max_move (poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) alpha) ; no more possible moves
    (t (let (
             (alpha (max alpha (alpha_beta (car poss_moves) (1- depth) alpha beta (next_player player)))))
         (if (<= beta alpha)
           alpha ; if true
           (get_max_move (cdr poss_moves) alpha beta depth player) ; else
           )))))

(defun get_min_move (poss_moves alpha beta rule depth player)
  (cond
    ((null poss_moves) beta)
    (t (let (
             (beta (min beta (alpha_beta (car poss_moves) (1- depth) alpha beta (next_player player)))))
         (if (<= beta alpha)
           beta ; if true
           (get_min_move (cdr poss_moves) alpha beta depth player) ; else
           )))))

(defun evaluate_board (board)
  10)

(defun max (a b)
  (if (> (car a) (car b))
    a ; if a>b
    b)); else if a<=b

(defun min (a b)
  (if (< (car a) (car b))
    a ; if a < b
    b)); else if a >= b

(defun get_next_rule (prev_rule)
  (cond
    ((equalp rule "max") "min")
    (t "max")))
