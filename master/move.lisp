#!/usr/bin/clisp

(load "creator")
(load "printer2")

(defun make_move (player position)
  (cond
    ((null position) '())
    (t (cond
         ((valid_move position) (insert_move player position))
         (t (princ (append (list "Invalid move -> ") position )))))))

(defun insert_move (player position)
  (cond
    ((null position) '())
    (t (setf (car (cdr (assoc (character (car position)) matrix)))
             (mapcar (lambda(element) (cond
                                        ((= (car element) (car position)) (list (car element) player))
                                        (t element)))
                     (car (cdr (assoc (character (car position)) matrix))))))))

(defun valid_move (next_position)
  (cond
    ((and (> (cadr next_position) (car (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (< (cadr next_position) (cadr (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (< (get_row (car next_position)) (1+ (* 2 (get_mat_dim matrix))))) t )
    (t '())))

(print_matrix)

(setq position (list #\C 4))

(princ position)

(princ (car (cdr (assoc (car position) matrix))))

(setf (car (cdr (assoc (car position) matrix)))
         (mapcar (lambda(element) (cond
                                    ((= (car element) (car position)) (list (car element) player))
                                    (t (list (car element) #\#))))
                 (car (cdr (assoc (character (car position)) matrix)))))

(print_matrix)


; (setq assls '((a (1 2 3)) (b (4 5 6)) (c (7 8 9))))
;
; (princ (assoc (setq choice (read)) assls))
;
; (princ choice)
;
; (princ (list assls #\linefeed))
;
; (setf (cadr (assoc choice assls)) (list 9 7 8 ))
;
; (princ assls)
