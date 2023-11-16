(load "funcs.lisp")
(load "boards.lisp")

(defvar example-size 6)
(setq empty-board (make-board example-size))
(setq sign-board (if (= example-size 9) sign-board-60 (if (= example-size 6 ) sign-board-23 sign-board-1)))

(replace-cell empty-board 1 0 '(4))

(print-board empty-board t)
(write (neighbors-numbers empty-board sign-board 1 0))