#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

(defun negamax(board depth alpha beta player)
	(cond 
		((or (= depth 0) (terminal_node board)) (get_eval board player))
		(t (get_max_move (possible_moves board player) alpha beta depth player))))

(defun get_max_move (poss_moves alpha beta depth player)
	(cond
		((null poss_moves) alpha)
		(t (let (
             (alpha (max alpha (- negamax (car poss_moves) (1- depth) (- beta) (- alpha) (next_player player)))))
         (if (>= alpha beta)
           alpha ; if true
           (get_max_move (cdr poss_moves) alpha beta depth player) ; else
           )))))

(defun max (a b)
  (if (> (car a) (car b))
    a ; if a>b
    b)); else if a<=b

(defun get_eval (board player)
	(cond ((equalp player #\X) (evaluate_board board))
		(t (- evaluate_board board))))