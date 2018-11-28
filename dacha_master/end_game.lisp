#!/usr/bin/clisp

(load "creator.lisp")
(load "move.lisp")
(load "printer.lisp")

(defun find_bridge(symbol row column)
  ())

(defun find_fork(symbol row column)
  ())

; (setq coords '((0 0)                                               ;gore levo
;               (0 (1- (get_mat_dim matrix)))                        ;gore desno
;               ((1- (get_mat_dim matrix)) 0)                               ;levo
;               ((1- (get_mat_dim matrix)) (* 2 (1- (get_mat_dim matrix))))        ;desno
;               ((* 2 (1- (get_mat_dim matrix))) (1- (get_mat_dim matrix)))        ;dole levo
;               ((* 2 (1- (get_mat_dim matrix))) (*2 (1- (get_mat_dim matrix))))   ;dole desno
;               ))

(defun check_corners(symbol)
  (progn
        (setq corners '(#\#))
        (cond
                   ((equalp (cadr (get_element 0 0)) symbol) (append corners #\#))
                   (t '()))
        (cond
                   ((equalp (cadr (get_element 0 (1- (get_mat_dim matrix)))) symbol) (cons corners (list 0 (1- (get_mat_dim matrix)))))
                   (t '()))
        (cond
                   ((equalp (cadr (get_element (1- (get_mat_dim matrix)) 0)) symbol) (cons corners (list (1- (get_mat_dim matrix)) 0)))
                   (t '()))
        (cond
                   ((equalp (cadr (get_element (1- (get_mat_dim matrix)) (* 2 (1- (get_mat_dim matrix))))) symbol) (cons (list (1- (get_mat_dim matrix)) (* 2 (1- (get_mat_dim matrix))))))
                   (t '()))
        (cond
                   ((equalp (cadr (get_element (* 2 (1- (get_mat_dim matrix))) (1- (get_mat_dim matrix)))) symbol) (cons (list (* 2 (1- (get_mat_dim))) (1- (get_mat_dim)))))
                   (t '()))
        (cond
                   ((equalp (cadr (get_element (* 2 (1- (get_mat_dim matrix))) (* 2 (1- (get_mat_dim matrix))))) symbol) (cons (list (* 2 (1- (get_mat_dim matrix))) (* 2 (1- (get_mat_dim matrix))))))
                   (t '()))
        ))

(defun check_corners2(symbol)
  (progn
   (setq corners '())
   (if (equalp (cadr (first (cadr (first matrix)))) symbol)
     (cons (list '0 '0) corners)
     )
   (if (equalp (cadr (last (cadr (first matrix)))) symbol)
     (append (list '0 (1- (get_mat_dim matrix))) corners))
   (if (equalp (cadr (first (cadr (nth (1- (get_mat_dim matrix)) matrix)))) symbol)
     (append (list (1- (get_mat_dim matrix)) '0) corners))
   (if (equalp (cadr (last (cadr (nth (1- (get_mat_dim matrix)) matrix)))) symbol)
     (cons (list (1- (get_mat_dim matrix)) (- (* 2 (get_mat_dim matrix)) 2)) corners))
   (if (equalp (cadr (first (cadr (last matrix)))) symbol)
     (cons (list (- (* 2 (get_mat_dim matrix)) 2) (1- (get_mat_dim matrix)) ) corners))
   (if (equalp (cadar (last (cadr (nth 10 matrix)))) symbol)
     (list (list (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2)) corners)
     )
   ))


(make_move 'X '0 '0)
(make_move 'X (- (* 2 (get_mat_dim matrix)) 2) (- (* 2 (get_mat_dim matrix)) 2))
(print_matrix)
(princ (check_corners2 'X))
