#!/usr/bin/clisp

(load "creator.lisp")
(load "printer.lisp")
(load "move.lisp")
(load "ascii.lisp")

(defun next_player(player)
  (cond
    ((equalp player #\X) #\O)
    (t #\X)))

(defun game(player)
  (let* (
         (po (mapcar 'princ (list "Unesite dve vrednosti: " #\linefeed)))
         (row (get_number (character (read))))
         (column (read))
         (valid (is_valid row column))
         (s_cond (cond
                  ((null valid) (let* (
                                       (po (mapcar 'princ (list "Izabrali ste nevalidnu poziciju, igrate ponovo..." #\linefeed)))
                                       (go (game player)))))
                  (t (let* (
                            (mmo (make_move player row column))
                            (cso (screen:clear-window (screen:make-window)))
                            (pmo (print_matrix)))))))
         (g (game (next_player player))))))
