#!/usr/bin/clisp

(let* (
       (a '(1 2 3 4))
       (b '(5 6 7 8))
       (l (let* (
                 (c '(10 11 12 13))))))
  (princ a))
