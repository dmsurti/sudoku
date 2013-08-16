;;;; cl-sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "package")
	       (:file "utils")
               (:file "sudoku")))

