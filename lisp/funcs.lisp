;; Ok
(defun is-valid (board sign-board)
  (and (is-full board) (check-rows board) (check-columns board) (check-blocks board) (check-signs boards sign-board)))

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

(defun check-signs board sign-board
  (let ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (check-signs-row board sign-board i))))

(defun check-signs-row board sign-board row
  (let ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (check-signs-cell board sign-board row i))))

;; Not ok
(defun check-signs-cell board sign-board row column
  (let* ((size (length board)))
    (loop for i from 0 to (- size 1)
          always (sign-valid ))))

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

(defun is-placement-valid (board sign-board row column value)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))

    ;; Função para verificar a validade da linha
    (defun check-row ()
      (loop for i from 0 below size
            for element = (nth i (nth row board))
            until (or (and (= (length element) 1) (= (first element) value) (/= i column))))

    ;; Função para verificar a validade da coluna
    (defun check-column ()
      (loop for i from 0 below size
            until (or (and (= (length (nth column (nth i board))) 1) (= (first (nth column (nth i board))) value) (/= i row))))

    ;; Função para verificar a validade do bloco
    (defun check-block ()
      (let* ((region-row (truncate row region-size-y))
             (region-column (truncate column region-size-x))
             (start-row (* region-row region-size-y))
             (end-row (+ start-row region-size-y))
             (start-column (* region-column region-size-x))
             (end-column (+ start-column region-size-x)))
        (loop for i from start-row below end-row
              append (loop for j from start-column below end-column
                           until (or (and (= (length (nth j (nth i board))) 1) (= (first (nth j (nth i board))) value) (/= (list i j) (list row column))))))))

    ;; Função para verificar a validade dos sinais
    (defun check-signs ()
      (let ((symbols (list (elt (elt sign-board row) column)))
           (neighbors-numbers (neighbors board sign-board row column)))
        (if (not (sign-valid value (first symbols) (second symbols) (third symbols) (fourth symbols) value (first neighbors-numbers) (second neighbors-numbers) (third neighbors-numbers) (fourth neighbors-numbers)))
            nil
            t)))

    ;; Chama as funções de verificação e retorna o resultado
    (and (check-row) (check-column) (check-block) (check-signs))))))

(defun test-is-placement-valid (board sign-board)
  (let* ((size (length board))
         (region-size-x (isqrt size))
         (region-size-y (truncate size region-size-x)))
    
    (loop for i from 0 below size do
      (loop for j from 0 below size do
        (loop for k from 0 below size do
          (let ((result (is-placement-valid board sign-board i j k)))
            (format t "is-placement-valid ~a, ~a, ~a: ~a~%" i j k (if result "True" "False"))))))))
