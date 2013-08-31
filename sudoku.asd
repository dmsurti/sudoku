;;;; cl-sudoku.asd

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "package")
	       (:file "utils")
               (:file "formats")
               (:file "sudoku")))

(asdf:defsystem #:sudoku-test
  :serial t
  :depends-on (#:sudoku)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))

(defmethod perform ((o test-op) (c (eql (find-system :sudoku))))
  (operate 'load-op :sudoku-test)
  (funcall (intern (symbol-name :run-all-tests) 
                   (find-package :sudoku-test))))
