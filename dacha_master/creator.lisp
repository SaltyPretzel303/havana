#!/usr/bin/clisp

(load "ascii.lisp")

(defstruct node
  ;column ; [ 0 - dimension ]
  value ; [ '- | 'X | 'O ]
  status) ; [ 'v | 's |'() ]
; v- status
; s- selected for visisting
; '()- not visited

; REPRESENTATION
#|
  nodeStruct:
        -value ['- | 'X | 'O]
        -status ['() | 'v | 's]

               ----------------- list of columns ---------------
  ((rowIndex (( columnIndex nodeStruct) (columnIndex nodeStruct)))
   (rowIndex  --||--))
|#

(defun create_matrix (dim)
  (cond
    ((< dim 0) '())
    (t (setq matrix (gen_row 0 dim)))))

(defun get_range(row dim)
  (cond
    ((< row 0) '(0 0))
    (t (cond
         ((< row dim) (list 0 (+ dim row)))
         (t (list (1+ (- row dim)) (- (* 2 dim) 1)))))))
; returns '(firstIndex lastIndex) for given row

(defun gen_pairs (index end)
  (cond
    ((= index end) '())
    (t (cons (list index (make-node
                           :value #\-
                           :status '()))
             (gen_pairs (1+ index) end)))))

(defun gen_row (row dim)
  (cond
    ((= row (1- (* 2 dim))) '())
    (t (cons (list row ; row index (letter in printing matrix)
                   (gen_pairs (car (get_range row dim)) (cadr (get_range row dim))))
             (gen_row (1+ row) dim)))))

; functions for matrix generation ^^^^

; functions helpers

(defun get_mat_dim (mat)
  (cond
    ((null mat) '())
    (t (1+ (floor (length mat) 2)))))
; return dimension of one side of playing field

(defun get_row (row)
  (cond
    ((null row) '())
    (t (assoc row matrix))))
; return row by its index

(defun get_column(index row)
  (cond
    ((null row) '())
    (t (assoc index (cadr row)))))
; return column with specified index from given row -> '(index (node :value :status))

(defun get_element(row column)
  (get_column column (get_row row)))
; returns element with given row and column -> '(columnIndex (node :value :status))

; functions for treversing nodes

(defun mark_as_visited(row column)
  (progn
    (if (boundp 'visited_nodes) ; boudnp checks does variable exists (is defined)
       (setf visited_nodes (cons (list row column) visited_nodes)); if exists append given cell (list row column)
       (setq visited_nodes (cons (list row column) '()))) ; if doesn't exists (first call)
    (setf (node-status (cadr (get_element row column))) #\v))) ; mark node as visited (#\v)

(defun reset_node (row column)
  (setf (node-status (cadr (get_element row column))) '())) ; set attribute :status on '() - neutral, not visited or selcted

(defun reset_visited_nodes(visited)
  (cond
    ((null visited) (setq visited_nodes '())) ; clear visited_nodes in the end
    (t (progn
          (princ (list "reseting: " (car visited))) ; debug message
          (reset_node (caar visited) (cadar visited)) ;
          (reset_visited_nodes (cdr visited)))))) ; call same function for the rest of list without first element


(defun mark_list_as_selected (ls)
  (progn (princ (list "selecting: " ls))
         (mapcar (lambda (element)
                  (progn
                   (mark_as_selected (car element) (cadr element)) ; marks element ass selected
                   element)) ; set same value for element in given list
                 ls)))

(defun is_visited (row column)
  (if (equalp (node-status (cadr (get_element row column))) #\v) ; get_elemnt returns-> (1 (node :value 'X :status 'v)) -> 'v' if visited
      t
    '()))

(defun mark_as_selected (row column)
  (setf (node-status (cadr (get_element row column))) #\s))

(defun is_selected (row column)
  (if (equalp (node-status (cadr (get_element row column))) #\s); get_elemnt returns-> (1 (node :value 'X :status 's)) -> 's' if selected for visiting
    t
    '()))

(create_matrix 6)
