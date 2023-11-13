;; Ok
(defun is-valid (board sign-board)
  (and (is-full board) (check-rows board) (check-columns board) (check-blocks board) (check-signs board sign-board)))

;; Ok
(defun is-full (board)
  (every (lambda (row) (every (lambda (cell) (= (length cell) 1)) row)) board))

;; Ok
(defun check-zone (zone)
  (cond
    ((null zone) t)
    ((null (cdr zone)) t)
    ((eq (car (car zone)) (car (cadr zone))) nil)
    (t (check-zone (cdr zone)))))

;; Ok
(defun board-row (board row)
  ;; return a copy of the row
  (copy-list (nth row board)))

(defun check-rows (board)
  (let ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (check-zone (sort (board-row board i) #'< :key #'car)))))

;; Ok
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

;; Ok
(defun check-blocks (board)
  (let* (
    (size (length board))
    (region-size-x (isqrt size))
    (region-size-y (truncate size region-size-x)))
    (loop for i from 0 to (- size 1)
          always (check-zone (sort (board-block board i) #'< :key #'car)))))

;; Ok
(defun check-signs-cell (board sign-board row column)
  (let* (
    (size (length board))
    (neighbor-signs (string-to-list(neighbors-signs sign-board row column)))
    (neighbor-numbers (neighbors-numbers board row column))
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
    t
  )
))

(defun check-signs-row (board sign-board row)
  (loop for i from 0 below (length board)
        always (check-signs-cell board sign-board row i)))

(defun check-signs (board sign-board)
  (loop for i from 0 below (length board)
        always (check-signs-row board sign-board i)))

;; Ok
(defun make-board (size)
  (if (= size 0)
      (list (list (list)))
      (loop repeat size
            collect (loop repeat size
                          collect (loop for i from 1 to size collect i)))))

;; Ok
(defun next-cell (position size)
  (if (< (second position) (- size 1))
      (cons (first position) (+ (second position) 1))
      (cons (+ (first position) 1) 0)))

;; Ok
(defun sign-valid (top right bottom left number top-num right-num bottom-num left-num)
  ; (write (list number top-num right-num bottom-num left-num))
  (cond
    ((and (string= top "^") (/= top-num 0) (<= number top-num)) nil)
    ((and (string= top "v") (/= top-num 0) (>= number top-num)) nil)
    ((and (string= right ">") (/= right-num 0) (<= number right-num)) nil)
    ((and (string= right "<") (/= right-num 0) (>= number right-num)) nil)
    ((and (string= bottom "v") (/= bottom-num 0) (<= number bottom-num)) nil)
    ((and (string= bottom "^") (/= bottom-num 0) (>= number bottom-num)) nil)
    ((and (string= left "<") (/= left-num 0) (<= number left-num)) nil)
    ((and (string= left ">") (/= left-num 0) (>= number left-num)) nil)
    (t t)))

;; Ok
(defun neighbors-signs (sign-board row column)
  (string (elt (elt sign-board row) column)))

;; Ok
(defun neighbors-numbers (board row column)
  (let* (
    (top (if (= row 0) 0 (if (= (length (nth column (nth row board))) 1) (first (nth column (nth (- row 1) board))) 0)))
    (right (if (= column (- (length board) 1)) 0 (if (= (length (nth column (nth row board))) 1) (first (nth (+ column 1) (nth row board))) 0)))
    (bottom (if (= row (- (length board) 1)) 0 (if (= (length (nth column (nth row board))) 1) (first (nth column (nth (+ row 1) board))) 0)))
    (left (if (= column 0) 0 (if (= (length (nth column (nth row board))) 1) (first (nth (- column 1) (nth row board))) 0)))
  )
  (list top right bottom left)))

;; Ok
(defun string-to-list (string)
  (loop for i from 0 below (length string)
        collect (string (elt string i))))

;; Ok
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

;; Ok
(defun count-signal (s signal)
  (if (string= signal s)
      1
      0))

;; Ok
(defun vergleich-cell-process (board sign-board row column)
  (let* (
    (neighbor-signs (string-to-list(neighbors-signs sign-board row column)))
    (pops (apply #'count-pops neighbor-signs))
    (cell (nth row (nth column board)))
  )
  (subseq cell (first pops) (- (length cell) (second pops)))))

;; Ok
(defun vergleich-row-process (board sign-board row)
  (loop for i from 0 below (length board)
        collect (vergleich-cell-process board sign-board row i)))

;; Ok
(defun vergleich-preprocess (board sign-board)
  (loop for i from 0 below (length board)
        collect (vergleich-row-process board sign-board i)))

       
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


;;Ok acho
(defun check-block-placement (board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (floor size region-size-x))
         (region-row (floor row region-size-y))
         (region-column (floor column region-size-x)))
    ;; Check blocks
    (loop for i from (* region-row region-size-y) below (* (1+ region-row) region-size-y)
          do (loop for j from (* region-column region-size-x) below (* (1+ region-column) region-size-x)
                   do (if (and (= (length (nth i board)) 1)
                                (= (nth 0 (nth i board)) value)
                                (not (and (= i row) (= j column))))
                          (return-from check-block-placement nil))))
    t))


(defun check-signs-placement (board sign-board row column value)
  (let* ((symbols (string-to-list (nth column (nth row sign-board))))
         (neighbors-numbers (neighbors-numbers board row column)))
    (if (not (sign-valid  (nth 0 symbols) (nth 1 symbols) (nth 2 symbols) (nth 3 symbols) value
                               (nth 0 neighbors-numbers) (nth 1 neighbors-numbers)
                               (nth 2 neighbors-numbers) (nth 3 neighbors-numbers)))
        nil
        t)))

(defun is-placement-valid (board sign-board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))
    (and (check-row-placement board row column value)
         (check-column-placement board row column value)
         (check-block-placement board row column value)
         (check-signs-placement board sign-board row column value))))

(defun test-is-placement-valid (board sign-board)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))
    
    (loop for i from 0 below size do
      (loop for j from 0 below size do
        (loop for k from 1 below size do
          (let ((result (is-placement-valid board sign-board i j k)))
            (format t "is-placement-valid ~a, ~a, ~a: ~a~%" i j k (if result "True" "False"))))))))

(defun print-possibilities (board)
  (loop for i from 0 below (length board)
        do (progn
             (format t "Layer ~a:~%" i)
             (loop for row in (nth i board)
                   do (progn
                        (loop for element in row
                              do (format t "~a " element))
                        (format t "~%"))))))

(defun print-board (board value)
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
