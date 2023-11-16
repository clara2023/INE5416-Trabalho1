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
  (if (= (cadr position) (1- size))
      (list (1+ (car position)) 0)
      (list (car position) (1+ (cadr position)))))

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
(defun neighbors-numbers (board sign-board row column)
  (let* (
    (signs (string-to-list(neighbors-signs sign-board row column)))
    (top-sign (first signs))
    (right-sign (second signs))
    (bottom-sign (third signs))
    (left-sign (fourth signs))
    (top (if (or (= row 0) (string= top-sign "."))
      0
      (if (string= top-sign "^")
          (car (nth column (nth (- row 1) board)))
          (car(last (nth column (nth (- row 1) board))))
      )
      ))
    (right (if (or (= column (- (length board) 1)) (string= right-sign "."))
      0
      (if (string= right-sign ">")
          (car (nth (+ column 1) (nth row board)))
          (car(last (nth (+ column 1) (nth row board))))
      )
      ))
    (bottom (if (or (= row (- (length board) 1)) (string= bottom-sign "."))
      0
      (if (string= bottom-sign "v")
          (car (nth column (nth (+ row 1) board)))
          (car(last (nth column (nth (+ row 1) board))))
      )
      ))
    (left (if (or (= column 0) (string= left-sign "."))
      0
      (if (string= left-sign "<")
          (car (nth (- column 1) (nth row board)))
          (car(last (nth (- column 1) (nth row board))))
      )
      ))
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

    ;(format t "type of symbols ~a~%" (type-of (nth 0 signs-to-sign-valid)))
    (if (not (sign-valid 
                         (nth 0 signs-to-sign-valid) (nth 1 signs-to-sign-valid) (nth 2 signs-to-sign-valid) (nth 3 signs-to-sign-valid) value
                         (nth 0 neighbors-n) (nth 1 neighbors-n) (nth 2 neighbors-n) (nth 3 neighbors-n)))
        nil
        t)))

(defun string-to-list-of-strings (input-string)
  "Converte uma string em uma lista de strings, onde cada string contém um caractere da string original."
  (mapcar #'string (coerce input-string 'list)))

(defun is-placement-valid (board sign-board row column value)
  (format t "is-placement-valid ~a, ~a, ~a~%" row column value)
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

;; Ok
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

(defun is-placement-valid (board sign-board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))
    (and (check-row-placement board row column value)
         (check-column-placement board row column value)
         (check-block-placement board row column value)
         (check-signs-placement board sign-board row column value))))

(defun replace-cell (matrix row column new-value)
  (setf (nth column (nth row matrix)) new-value))

(defun solve (board sign-board &optional (row 0) (column 0))
  (if (is-full board)
      (if (is-valid board sign-board)
          board
          nil
          )
      (let* ((next-cell (next-cell (list row column) (length board)))
            (next-row (car next-cell))
            (next-column (cadr next-cell)))
        (if (= (length (nth column (nth row board))) 0)
            nil
            (loop for i in (nth column (nth row board))
                  do (let ((copy (copia board)))
                      (if (is-placement-valid copy sign-board row column i)
                          (progn
                            (replace-cell copy row column (list i))
                            (let ((result (solve copy sign-board next-row next-column)))
                              (if result
                                  (return-from solve result)
                                  nil
                                  )
                              )
                            )
                          nil
                          )
                            ))))))

(defun copia (board)
  (loop for row in board
        collect (loop for element in row
                      collect element)))

(defun pre-process (board sign-board)
  (let* ((size (length board))
         (copy (copia board))
         (success t)
         (total 0))
    (loop while success do
          (setq total (+ total 1)
                success nil)
          (loop for row below size do
                (loop for column below size do
                      (if (eql 1 (length (nth column (nth row copy))))
                          (continue))
                      (loop for k in (nth column (nth row copy)) do
                            (if (not (is-placement-valid copy sign-board row column k))
                                (progn
                                  (setf (nth column (nth row copy)) (remove k (nth column (nth row copy)))
                                        success t)
                                        ))))))
    copy))


