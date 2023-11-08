(load "funcs.lisp")

;; Ok
(setq sign-board
      '((".>v." "..v>"   ".>^." "..v>"   ".<^." "..v<")
        ("v<v." "v.^<"   "^>v." "v.^>"   "^<v." "v.v<")
        ("v<.." "^..<"   "v>.." "^..>"   "v<.." "v..<")

        (".<^." "..v<"   ".<^." "..v<"   ".>^." "..^>")
        ("^>v." "v.^>"   "^<^." "v.^<"   "^>^." "^.v>")
        ("v>.." "^..>"   "^<.." "^..<"   "^>.." "v..>")))

;; OK
(setq board
  '(((2) (3) (1) (4))
    ((1) (4) (3) (2))
    ((4) (1) (2) (3))
    ((3) (2) (4) (1))
  )
)

;; (write (board-block board 0 1))
(write (check-blocks board))
;; (write (check-zone (board-block board 1 2)))