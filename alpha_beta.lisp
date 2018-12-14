#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

; returns (evaluation (row, column))
(defun alpha_beta(board alpha beta depth player)
  (progn (princ (list "albeta -> " player #\linefeed))
         (princ #\linefeed)
         (cond
           ((or (= depth 0) (terminal_node board)) (evaluate_board board)) ; terminal_node -> no more valid moves
           (t (cond
               ((equalp player #\X)
                (progn (princ board) (princ #\linefeed) (get_max_move board (possible_moves board) alpha beta depth player)))
               ((equalp player #\O)
                (progn (princ board) (princ #\linefeed) (get_min_move board (possible_moves board) alpha beta depth player))))))))

(defun get_max_move (board poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) alpha) ; no more possible moves
    (t (let (
              (p2 (princ "maxMoves-> "))
              (princ #\linefeed)
              (alpha (max alpha (alpha_beta (cdr (next_state board player (caar poss_moves) (cadar poss_moves))) alpha beta (1- depth) (next_player player)))))
         (if (<= beta alpha)
           alpha ; if true
           (get_max_move board (cdr poss_moves) alpha beta depth player)))))) ; else


(defun get_min_move (board poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) beta)
    (t (let (
              (p2 (princ "minMoves-> "))
              (p2 (princ #\linefeed))
              (beta (min beta (alpha_beta (cdr(next_state board player (caar poss_moves) (cadar poss_moves))) alpha beta (1- depth) (next_player player)))))
         (if (<= beta alpha)
             beta ; if true
             (get_min_move board (cdr poss_moves) alpha beta depth player)))))) ; else

(defun terminal_node (board)
  '())


(defun evaluate_board (board)
  (list 10 (list 0 0)))

(defun max (a b)
  (if (> (car a) (car b))
    a ; if a>b
    b)); else if a<=b

(defun min (a b)
  (if (< (car a) (car b))
    a ; if a < b
    b)); else if a >= b
