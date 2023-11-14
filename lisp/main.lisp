(load "funcs.lisp")

;; Ok
(setq sign-board2
      '((".>v." "..v>"   ".>^." "..v>"   ".<^." "..v<")
        ("v<v." "v.^<"   "^>v." "v.^>"   "^<v." "v.v<")
        ("v<.." "^..<"   "v>.." "^..>"   "v<.." "v..<")

        (".<^." "..v<"   ".<^." "..v<"   ".>^." "..^>")
        ("^>v." "v.^>"   "^<^." "v.^<"   "^>^." "^.v>")
        ("v>.." "^..>"   "^<.." "^..<"   "^>.." "v..>")))

(setq sign-board
  '(
    (".<v." "..^<"    ".<^." "..v<")
    ("v<.." "^..<"    "^>.." "v..>")

    (".>v." "..^>"    ".<^." "..v<")
    ("v>.." "^..>"    "^>.." "v..>")
  )
)

;; OK
(setq board
  '(((2) (3) (1) (4))
    ((1) (4) (3) (2))
    ((4) (1) (2) (3))
    ((3) (2) (4) (2 1))
  )
)
; (write (check-signs-row board sign-board 0))
(setq my-board (make-board 6))
(setq processed-board (vergleich-preprocess my-board sign-board2))
(print-board processed-board t)
(setq solved (solve processed-board sign-board2))
(if solved
    (print-board solved t)
    (write "No solution found"))