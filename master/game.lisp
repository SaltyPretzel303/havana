#!/usr/bin/clisp

(load "creator")
(load "printer2")

(princ "Do you want to play first (y/n)")
(setq player (read))
(screen:clear-window (screen:make-window))
(cond
  ((equalp 'n player)))(player_move)
  (t (computer_move))

(defun player_move ()
  (cond ()))

(defun computer_move()
  ())
