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
   (t (setf (car (cdr (assoc (car position) matrix)))
            (mapcar (lambda(element) (cond
                                       ((= (car element) (cadr position)) (list (car element) player))
                                       (t element)))
                    (car (cdr (assoc (car position) matrix))))))))

(defun valid_move (next_position)
  (cond
    ((and (>= (cadr next_position) (car (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (<= (cadr next_position) (cadr (get_range (get_row (car next_position)) (get_mat_dim matrix))))
         (< (get_row (car next_position)) (1+ (* 2 (get_mat_dim matrix))))) t )
    (t '())))

(create_p_mat matrix)
(print_matrix)

(princ #\linefeed)
(princ #\linefeed)

(make_move #\# (list (read) 4) )

(gen_print_matrix matrix)
; (setq print_mat (cons
;                  (append (create_padding (1+ (calc_padding_size (code-char (1- (char-code #\A))) (get_mat_dim matrix))))
;                        (append (range 0 (get_mat_dim matrix)) (list #\linefeed)))
;                  print_mat))

(create_p_mat matrix)

(print_matrix)
