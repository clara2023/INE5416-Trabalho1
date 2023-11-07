(defun isFull (board)
  (every (lambda (row) (every (lambda (cell) (= (length cell) 1)) row)) board))

(defun check-rows (board)
  (let ((size (length board)))
    (defun check-row (row)
      (cond
        ((null row) t)
        ((null (cdr row)) t)
        ((eq (car row) (cadr row)) nil)
        (t (check-row (cdr row)))))
    (every (lambda (i)
             (check-row (sort (nth i board))))
           (loop for i from 0 to (- size 1)))))

(check-rows board)

(defun check-columns (board)
  (let ((size (length board)))
    (defun check-column (column)
      (cond
        ((null column) t)
        ((null (cdr column)) t)
        ((eq (car column) (cadr column)) nil)
        (t (check-column (cdr column)))))
    (every #'check-column
           (loop for i from 0 to (- size 1)
                 collect (sort (loop for j from 0 to (- size 1)
                                     collect (nth j (nth i board))))))))

(defun check-block (lst)
  (cond ((null lst) t)
        ((null (cdr lst)) t)
        ((eq (car lst) (cadr lst)) nil)
        (t (check-block (cdr lst)))))

(defun check-blocks (board)
  (let* ((size (length board))
         (region-size-x (floor (sqrt (float size))))
         (region-size-y (/ size region-size-x)))
    (loop for i from 0 to (- size 1) by region-size-y
          always (loop for j from 0 to (- size 1) by region-size-x
                       always (check-block (sort (loop for x from i to (+ i (- region-size-y 1))
                                                       append (loop for y from j to (+ j (- region-size-x 1))
                                                                    collect (nth y (nth x board))))))))))

(check-blocks '((1 2 3 4)
                (2 3 4 1)
                (3 4 1 2)
                (4 1 2 3)))



