(load "auxiliar.lisp")

; Remove os extremos de cada célula baseado nos sinais vizinhos
(defun vergleich-preprocess (board sign-board)
  (loop for i from 0 below (length board)
        collect (vergleich-row-process board sign-board i)))

(defun vergleich-row-process (board sign-board row)
  (loop for i from 0 below (length board)
        collect (vergleich-cell-process board sign-board row i)))

(defun vergleich-cell-process (board sign-board row column)
  (let* (
    (neighbor-signs (string-to-list(neighbors-signs sign-board row column)))
    (pops (apply #'count-pops neighbor-signs))
    (cell (nth row (nth column board)))
  )
  (subseq cell (first pops) (- (length cell) (second pops)))))
;;

; Testa possibilidades de forma otimizada, até encontrar alguma válida
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
                  do (let ((copy (pre-process board sign-board)))
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

; Remove as possibilidades inválidas de cada célula
(defun pre-process (board sign-board)
  (let* ((size (length board))
         (copy (copy-board board))
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
                                  (setf (nth column (nth row copy)) (remove k (nth column (nth row copy)))
                                        success t)
                                        )))))
    copy
    ))

;; Busca o valor possível menos limitante para cada vizinho baseado nos sinais
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
  (list top right bottom left)
  ))
