(load "funcs.lisp")
(load "boards.lisp")

(defvar example-size 6)
(setq empty-board (make-board example-size))
(setq sign-board (if (= example-size 9) sign-board-60 (if (= example-size 6 ) sign-board-23 sign-board-1)))

(print-board empty-board nil)
(setq vergleich-board (vergleich-preprocess empty-board sign-board))
(setq vergleich-board (make-board example-size))
(print-board vergleich-board nil)

(replace-cell vergleich-board 0 0 '(5))
(setq processed-board (pre-process vergleich-board sign-board))
(print-board processed-board nil)
