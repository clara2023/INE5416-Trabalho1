;; Funcionalidades
(defun is-placement-valid (board sign-board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))
    (and (check-row-placement board row column value)
         (check-column-placement board row column value)
         (check-block-placement board row column value)
         (check-signs-placement board sign-board row column value))))

(defun check-row-placement (board row column value)
  (loop for i from 0 to (1- (length (nth row board)))
        for element = (nth i (nth row board))
        when (and (= (length element) 1) (= (first element) value) (not (= i column)))
        return nil
        finally (return t)))

(defun check-column-placement (board row column value)
  (let ((size (length board)))
    (loop for i from 0 to (1- size)
          for element = (nth column (nth i board))
          when (and (= (length element) 1) (= (first element) value) (not (= i row)))
          return nil
          finally (return t))))

(defun check-block-placement (board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (floor size region-size-x))
         (region-row (floor row region-size-y))
         (region-column (floor column region-size-x)))
    (loop for i from (* region-row region-size-y) below (* (1+ region-row) region-size-y)
          do (loop for j from (* region-column region-size-x) below (* (1+ region-column) region-size-x)
                   do (
                    if (and (= (length (nth j (nth i board))) 1)
                               (not (and (= i row) (= j column)))
                               (eql (car(nth j (nth i board))) value)
                                )
                          (return-from check-block-placement nil)
                          )))
    t))

(defun check-signs-placement (board sign-board row column value)
  (let* ((symbols (string-to-list (nth column (nth row sign-board))))
         (neighbors-n (neighbors-numbers board sign-board row column))
         (signs-to-sign-valid (string-to-list-of-strings symbols)))

    (if (not (sign-valid 
                         (nth 0 signs-to-sign-valid) (nth 1 signs-to-sign-valid) (nth 2 signs-to-sign-valid) (nth 3 signs-to-sign-valid) value
                         (nth 0 neighbors-n) (nth 1 neighbors-n) (nth 2 neighbors-n) (nth 3 neighbors-n)))
        nil
        t)))

; Checa se a solução é válida
(defun is-valid (board sign-board)
  (and (is-full board) (check-rows board) (check-columns board) (check-blocks board) (check-signs board sign-board)))

(defun is-full (board)
  (every (lambda (row) (every (lambda (cell) (= (length cell) 1)) row)) board))

(defun check-zone (zone)
  (cond
    ((null zone) t)
    ((null (cdr zone)) t)
    ((eq (car (car zone)) (car (cadr zone))) nil)
    (t (check-zone (cdr zone)))))

(defun board-row (board row)
  (copy-list (nth row board)))

(defun check-rows (board)
  (let ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (check-zone (sort (board-row board i) #'< :key #'car)))))

(defun board-column (board column)
  (loop for row in board
        collect (nth column row)))

(defun check-columns (board)
  (let ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (check-zone (sort (board-column board i) #'< :key #'car)))))

(defun board-block (board block)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x))
         (block-x (mod block region-size-x))
         (block-y (truncate block region-size-y))
         (start-row (* block-y region-size-y))
         (end-row (+ start-row region-size-y))
         (start-column (* block-x region-size-x))
         (end-column (+ start-column region-size-x)))
    (loop for i from start-row below end-row
          append (loop for j from start-column below end-column
                       collect (nth j (nth i board))))))

(defun check-blocks (board)
  (let* (
    (size (length board))
    (region-size-x (isqrt size))
    (region-size-y (truncate size region-size-x)))
    (loop for i from 0 to (- size 1)
          always (check-zone (sort (board-block board i) #'< :key #'car)))))

(defun check-signs (board sign-board)
  (loop for i from 0 below (length board)
        always (check-signs-row board sign-board i)))

(defun check-signs-row (board sign-board row)
  (loop for i from 0 below (length board)
        always (check-signs-cell board sign-board row i)))

(defun check-signs-cell (board sign-board row column)
  (let* (
    (size (length board))
    (neighbor-signs (string-to-list(neighbors-signs sign-board row column)))
    (neighbor-numbers (neighbors-numbers board sign-board row column))
    (top (first neighbor-signs))
    (right (second neighbor-signs))
    (bottom (third neighbor-signs))
    (left (fourth neighbor-signs))
    (number (nth column (nth row board)))
    (top-num (first neighbor-numbers))
    (right-num (second neighbor-numbers))
    (bottom-num (third neighbor-numbers))
    (left-num (fourth neighbor-numbers))
  )
  (if (= (length number) 1)
    (sign-valid top right bottom left (car number) top-num right-num bottom-num left-num)
    t)))

(defun sign-valid (top right bottom left number top-num right-num bottom-num left-num)
  (cond
    ((not (and top-num right-num left-num bottom-num)) nil)
    ((and (string= top "^") (/= top-num 0) (<= number top-num)) nil)
    ((and (string= top "v") (/= top-num 0) (>= number top-num)) nil)
    ((and (string= right ">") (/= right-num 0) (<= number right-num)) nil)
    ((and (string= right "<") (/= right-num 0) (>= number right-num)) nil)
    ((and (string= bottom "v") (/= bottom-num 0) (<= number bottom-num)) nil)
    ((and (string= bottom "^") (/= bottom-num 0) (>= number bottom-num)) nil)
    ((and (string= left "<") (/= left-num 0) (<= number left-num)) nil)
    ((and (string= left ">") (/= left-num 0) (>= number left-num)) nil)
    (t t)))

;; Detalhes
(defun make-board (size)
  (if (= size 0)
      (list (list (list)))
      (loop repeat size
            collect (loop repeat size
                          collect (loop for i from 1 to size collect i)))))

(defun next-cell (position size)
  (if (= (cadr position) (1- size))
      (list (1+ (car position)) 0)
      (list (car position) (1+ (cadr position)))))

(defun copy-board (board)
  (loop for row in board
        collect (loop for element in row
                      collect element)))

(defun replace-cell (matrix row column new-value)
  (setf (nth column (nth row matrix)) new-value))

(defun neighbors-signs (sign-board row column)
  (string (elt (elt sign-board row) column)))

(defun count-pops (top right bottom left)
  (let* ((pop-front (+ (count-signal "^" top)
                      (count-signal ">" right)
                      (count-signal "v" bottom)
                      (count-signal "<" left)))
         (pop-back (+ (count-signal "v" top)
                      (count-signal "<" right)
                      (count-signal "^" bottom)
                      (count-signal ">" left))))

    (list pop-front pop-back)))

(defun count-signal (s signal)
  (if (string= signal s)
      1
      0))

(defun string-to-list (string)
  (loop for i from 0 below (length string)
        collect (string (elt string i))))

(defun string-to-list-of-strings (input-string)
  "Converte uma string em uma lista de strings, onde cada string contém um caractere da string original."
  (mapcar #'string (coerce input-string 'list)))

;; Output
(defun print-board (board value)
  (write "=====================")
  (terpri)
  (let* ((size (length board))
         (region-size-x (floor (sqrt size)))
         (region-size-y (truncate size region-size-x)))
    (print-board-rows board size region-size-x region-size-y value)))

(defun print-board-rows (board size region-size-x region-size-y value)
  (if (= size 0)
      (return-from print-board-rows))
  (print-row (car board) region-size-x value)
  (format t "~%")
  (when (= (mod (- size 1) region-size-y) 0)
    (format t "~%"))
  (print-board-rows (cdr board) (- size 1) region-size-x region-size-y value))

(defun print-row (row region-size-x value)
  (if (endp row)
      (return-from print-row))
  (print-cell (car row) value)
  (when (= (mod (- (length row) 1) region-size-x) 0)
    (princ " "))
  (print-row (cdr row) region-size-x value))

(defun print-cell (cell value)
  (if value
    (if (not (= (length cell) 1))
        (princ "□ ")
        (princ (format nil "~a " (first cell))))
    (princ (format nil "~a " (length cell)))))
;;

(defun print-possibilities (board)
  (loop for i from 0 below (length board)
        do (progn
             (format t "Layer ~a:~%" i)
             (loop for row in (nth i board)
                   do (progn
                        (loop for element in row
                              do (format t "~a " element))
                        (format t "~%"))))))

