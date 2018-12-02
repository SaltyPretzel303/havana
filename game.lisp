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

(defun player_part_game(symbol)
  (let* (
         (po (mapcar 'princ (list "Unesite dve vrednosti: (player)" #\linefeed)))
         (row (get_number (character (read))))
         (column (read))
         (valid (is_valid row column))
         (s_cond (cond
                  ((null valid) (let* ( ; invalid move
                                       (po (mapcar 'princ (list "Izabrali ste nevalidnu poziciju, igrate ponovo... (player)" #\linefeed)))
                                       (go (player_part_game symbol)))))
                  (t (let* ( ; valid move
                            (mmo (make_move symbol row column)) ; make move
                            (bridge_created (check_bridge symbol)) ; check for bridge
                            (fork_created (check_fork (list row column)))
                            (ring_created (check_ring (list row column)))
                            (next (if (or (not (null bridge_created)) (not (null fork_created)) (not (null ring_created)))
                                    (progn ; bridge created, end game
                                      (screen:clear-window (screen:make-window))
                                      (print_matrix)
                                      (mapcar 'princ (list "Game finished" #\linefeed "Winer is: " symbol #\linefeed "Reason: "))
                                      (cond
                                        ((not (null bridge_created)) (princ "bridge"))
                                        ((not (null fork_created)) (princ "fork"))
                                        ((not (null ring_created)) (princ "ring"))))
                                    (progn ; continue with game
                                      (screen:clear-window (screen:make-window))
                                      (print_matrix)
                                      (computer_part_game (next_player symbol)))))))))))))


(defun computer_part_game (symbol)
  (let* (
         (new_state (compute_next_move symbol))
         (played (car new_state))
         (new_state (make_move_sample (cdr new_state)))
         (s (let* (
                    (bridge (check_bridge symbol))
                    (fork (check_fork played))
                    (ring (check_ring played)))
               (if (or (not (null bridge)) (not (null fork)) (not (null ring)))
                 (progn
                   (screen:clear-window (screen:make-window))
                   (print_matrix)
                   (mapcar 'princ (list "Game finished" #\linefeed "Winner is: " symbol #\linefeed "Reason: "))
                   (cond
                     ((not(null bridge)) (princ "bridge"))
                     ((not(null fork)) (princ "fork"))
                     ((not(null ring)) (princ "ring"))))
                 (progn
                   (screen:clear-window (screen:make-window))
                   (print_matrix)
                   (player_part_game (next_player symbol)))))))))

(player_part_game #\X)
