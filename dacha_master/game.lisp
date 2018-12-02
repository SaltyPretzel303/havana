#!/usr/bin/clisp

(load "creator.lisp")
(load "printer.lisp")
(load "move.lisp")
(load "ascii.lisp")
(load "end_game.lisp")

; (defun set_upper_left_wall()'())

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
                  ((null valid) (let* ( ; invalid move
                                       (po (mapcar 'princ (list "Izabrali ste nevalidnu poziciju, igrate ponovo..." #\linefeed)))
                                       (go (game player)))))
                  (t (let* ( ; valid move
                            (mmo (make_move player row column)) ; make move
                            (bridge_created (check_bridge player)) ; check for bridge
                            (fork_created (check_fork (list row column)))
                            (ring_created '())
                            (next (if (or (not (null bridge_created)) (not (null fork_created)) (not (null ring_created)))
                                    (progn ; bridge created, end game
                                      (screen:clear-window (screen:make-window))
                                      (print_matrix)
                                      (mapcar 'princ (list "Game finished" #\linefeed "Winer is: " player #\linefeed "Reason: "))
                                      (cond
                                        ((not (null bridge_created)) (princ "bridge"))
                                        ((not (null fork_created)) (princ "fork"))
                                        ((not (null ring_created)) (princ "ring"))))
                                    (progn ; continue with game
                                      (screen:clear-window (screen:make-window))
                                      (print_matrix)
                                      (game (next_player player)))))))))))))

(game #\x)
