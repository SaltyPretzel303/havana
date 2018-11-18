

(load "printer2.lisp")
(load "creator.lisp")

(defun calculateRow (letter)
  (- (char-code letter) 65))

(defun move2 (l vrednost kolona)
  (cond
    ((null l) '())
    ((= kolona (caar l)) (cons (list (caar l) vrednost) (move2 (cdr l) vrednost kolona)))
    (t (cons (car l) (move2 (cdr l) vrednost kolona)))))

(defun move(matrix vrednost vrsta kolona)
  (cond 
    ((null matrix) '())
    ((= (calculateRow vrsta) (calculateRow (caar matrix))) (cons (cons (caar matrix) (move2 (cadar matrix) vrednost kolona)) (move (cdr matrix) vrednost vrsta kolona)))
    (t (cons (car matrix) (move (cdr matrix) vrednost vrsta kolona)))))

(move matrix 'X #\B 2)

(print_matrix)






