#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")

; returns (evaluation (row, column))
; board -> (playedField (rest of the regular board representation))
(defun alpha_beta(curr_move alpha beta depth player)
         (cond
           ((or (= depth 0) (terminal_node)) (evaluate_board curr_move player)) ; terminal_node -> no more valid moves
           (t (cond
               ((equalp player #\X)
                ; (cdr board) is just the board without first element (which is played field)
                (progn (get_max_move curr_move (possible_moves) alpha beta depth player)))
               ((equalp player #\O)
                (progn (get_min_move curr_move (possible_moves) alpha beta depth player)))))))

; board -> (playedField (rest of the board))
(defun get_max_move (curr_move poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) alpha); no more possible moves
    (t (let* (
              (next_s (make_move player (caar poss_moves) (cadar poss_moves)))
              (value (alpha_beta (car poss_moves) alpha beta (1- depth) (next_player player)))
              (undo (undo_move (caar poss_moves) (cadar poss_moves)))
              (fi (if (> (car value) (car alpha))
                    (setf alpha (list (car value) (car poss_moves))))))
         (if (<= (car beta) (car alpha)) ; format of alpha & beta (evaluation (row, column))
           alpha ; if true
           (get_max_move curr_move (cdr poss_moves) alpha beta depth player)))))) ; else


(defun get_min_move (curr_move poss_moves alpha beta depth player)
  (cond
    ((null poss_moves) beta)
    (t (let* (
               (next_s (make_move player (caar poss_moves) (cadar poss_moves)))
               (value (alpha_beta (car poss_moves) alpha beta (1- depth) (next_player player)))
               (undo (undo_move (caar poss_moves) (cadar poss_moves)))
               (fi (if (< (car value) (car beta))
                     (setf beta (list (car value) (car poss_moves))))))
             (if (<= (car beta) (car alpha)) ; format of alpha& beta (evaluation (row, column))
                 beta ; if true
               (get_min_move curr_move (cdr poss_moves) alpha beta depth player)))))) ; else

(defun terminal_node ()
  (equalp 0 (length (possible_moves))))


(defun evaluate_board (curr_move player)
  (let (
        (ret_value (list '0 (list curr_move)))
        (bridge (check_bridge player))
        (fork (check_fork curr_move))
        (ring (check_ring curr_move)))
    (if (or bridge fork ring)
      (progn
       (princ (list "naso je " bridge fork ring #\linefeed))
       (princ matrix)
       (princ #\linefeed)
       (read)
       (if (equalp player #\X)
        (list '999 (list curr_move))
        (list '-999 (list curr_move))))
      ret_value)))

(defun max_eval (a b)
  (if (>= (car a) (car b))
    a ; if a>b
    b)); else if a<=b

(defun min_eval (a b)
  (if (<= (car a) (car b))
    a ; if a < b
    b)); else if a >= b
