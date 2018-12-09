#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

(defun alpha_beta(board alpha beta depth rule player) ; rule -> min || max
  (cond
    ((or (= depth 0) (terminal_node board)) (evaluate_board temp_board)) ; terminal_node -> no more valid moves
    (t (cond
        ((= rule "max")
            (let (
                  (poss_moves (possible_moves board player))
                  (max_move (get_max_move poss_moves alpha beta rule depth player))
                 )
             max_move ; return from let
            )
          )
        ((= rule "min")
            (let (
                  (pos_moves (possible_moves board player))
                  (min_move (get_min_move poss_moves alpha beta rule depth player))
                 )
             min_move ; return from let
            )
          )
         ))))

(defun get_max_move (poss_moves alpha beta rule depth player)
  (cond
    ((null poss_moves) alpha) ; no more possible moves
    (t (let (
             (alpha (max alpha (alpha_beta (car poss_moves) (1- depth) alpha beta (get_next_rule rule) player))))
         (if (<= beta alpha)
           alpha ; if true
           (max_move (cdr poss_moves) alpha beta rule depth player) ; else
           )))))

(defun get_min_move (poss_moves alpha beta rule depth player)
  (cond
    ((null poss_mvoes) beta)
    (t (let (
             (beta (min beta (alpha_beta (car poss_moves) (1- depth) alpha beta (get_next_rule rule) player))))
         (if (<= beta alpha)
           beta
           (min_move (cdr poss_moves) alpha beta rule depth player)
           )))))

(defun evaluate_board (board))

(defun max (a b)
  (if (> a b)
    a ; if a>b
    b)); else if a<=b

(defun min (a b)
  (if (< a b)
    a ; if a < b
    b)); else if a >= b

(defun get_next_rule (prev_rule)
  (cond
    ((equalp rule "max") "min")
    (t "max")))
