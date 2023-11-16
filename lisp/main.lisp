(load "funcs.lisp")
(load "boards.lisp")

(defvar example-size 6)

(setq sign-board (if (= example-size 9) sign-board-60 (if (= example-size 6 ) sign-board-23 sign-board-1)))

(setq empty-board (make-board example-size))
(setq vergleich-preprocessed-board (vergleich-preprocess empty-board sign-board))

(print-board vergleich-preprocessed-board nil)

(setq preprocessed-board (pre-process vergleich-preprocessed-board sign-board))
(print-board preprocessed-board nil)

(setq solved (solve vergleich-preprocessed-board sign-board))
(if solved
    (print-board solved t)
    (write "No solution found"))