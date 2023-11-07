(defun is-valid (board)
  (and (is-full board) (check-rows board) (check-columns board) (check-blocks board)))


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
(setq sign-board
      '((".>v." "..v>"   ".>^." "..v>"   ".<^." "..v<")
        ("v<v." "v.^<"   "^>v." "v.^>"   "^<v." "v.v<")
        ("v<.." "^..<"   "v>.." "^..>"   "v<.." "v..<")

        (".<^." "..v<"   ".<^." "..v<"   ".>^." "..^>")
        ("^>v." "v.^>"   "^<^." "v.^<"   "^>^." "^.v>")
        ("v>.." "^..>"   "^<.." "^..<"   "^>.." "v..>")))

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

;; String to tuple não funciona em lisp
;; A atribuição tem que ser na mão

;; Ok
(defun neighbors-signs (sign-board row column)
  (string (elt (elt sign-board row) column)))

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

    (values pop-front pop-back)))

;; Ok
(defun count-signal (s signal)
  (if (string= signal s)
      1
      0))


;; uuuuuh bruh it kinda works
(defun vergleich-preprocess (board sign-board)
  (let* ((size (length board)))
         (count-pops (loop for row from 0 to (- size 1)
                    collect (loop for column from 0 to (- size 1)
                             collect (string-to-tuple (aref (aref (sign-board-signs sign-board) row) column))))
         (process-row (lambda (row row-pops)
                        (loop for cell in row
                           collect (process-cell cell (elt row-pops (length row-pops))))))
         (process-cell (lambda (cell pop)
                         (let* ((pop-front-back (count-pops (first pop) (second pop) (third pop) (fourth pop)))
                                (pop-front (first pop-front-back))
                                (pop-back (second pop-front-back)))
                           (nreverse (nthcdr pop-back (nreverse (nthcdr pop-front cell))))))))
    (make-board :cells (loop for row in (board-cells board)
                          collect (process-row row pops)))))

;; Ok
(defun next-cell (coordinates size)
  (if (< (cdr coordinates) (- size 1))
      (cons (car coordinates) (+ (cdr coordinates) 1))
      (cons (+ (car coordinates) 1) 0)))

;;O símbolo # seguido por um caractere é usado para criar um objeto do tipo caractere em Lisp.
;; Portanto, #\^ cria um objeto caractere que representa a seta para cima.

;; Ok
(defun count-pops (top right bottom left)
  (let ((pop-front (+ (count-signal #\^ top)
                      (count-signal #\> right)
                      (count-signal #\v bottom)
                      (count-signal #\< left)))
        (pop-back (+ (count-signal #\v top)
                     (count-signal #\< right)
                     (count-signal #\^ bottom)
                     (count-signal #\> left))))
    (values pop-front pop-back)))

;; Exemplo de uso:
;; (multiple-value-bind (front back)
;;     (count-pops #\^ #\< #\^ #\<)
;;   (format t "Pop Front: ~a, Pop Back: ~a" front back))
