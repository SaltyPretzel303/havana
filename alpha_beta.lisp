#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

; returns (evaluation (row, column))
; board -> (playedField (rest of the regular board representation))
(defun alpha_beta(board alpha beta depth player)
         (cond
           ((or (= depth 0) (terminal_node (cdr board))) (evaluate_board board)) ; terminal_node -> no more valid moves
           (t (cond
               ((equalp player #\X)
                ; (cdr board) is just the board without first element (which is played field)
                (progn (get_max_move board (possible_moves (cdr board)) alpha beta depth player)))
               ((equalp player #\O)
                (progn (get_min_move board (possible_moves (cdr board)) alpha beta depth player)))))))


; board -> (playedField (rest of the board))
(defun get_max_move (board poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) alpha) ; no more possible moves
    (t (let (
              (alpha (max_eval alpha (alpha_beta (next_state (cdr board) player (caar poss_moves) (cadar poss_moves)) alpha beta (1- depth) (next_player player)))))
         (if (<= (car beta) (car alpha)) ; format of alpha& beta (evaluation (row, column))
           alpha ; if true
           (get_max_move board (cdr poss_moves) alpha beta depth player)))))) ; else


(defun get_min_move (board poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) beta)
    (t (let (
              (beta (min_eval beta (alpha_beta (next_state (cdr board) player (caar poss_moves) (cadar poss_moves)) alpha beta (1- depth) (next_player player)))))
         (if (<= (car beta) (car alpha)) ; format of alpha& beta (evaluation (row, column))
             beta ; if true
             (get_min_move board (cdr poss_moves) alpha beta depth player)))))) ; else

(defun terminal_node (board)
  (equalp 0 (length (possible_moves board))))


(defun evaluate_board (board)
     (list -10 (car board))) ; (car board) -> played filed

(defun max_eval (a b)
  (if (> (car a) (car b))
    a ; if a>b
    b)); else if a<=b

(defun min_eval (a b)
  (if (< (car a) (car b))
    a ; if a < b
    b)); else if a >= b
