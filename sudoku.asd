;;;; *************
;;;; Source system
;;;; *************

(asdf:defsystem #:sudoku
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "package")
	       (:file "utils")
               (:file "formats")
               (:file "sudoku")))

;;;; ***********
;;;; Test system
;;;; ***********

(asdf:defsystem #:sudoku-test
  :serial t
  :depends-on (#:sudoku #:cl-fad)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tests")))))

;;;; *****************************
;;;; Define path to test resources
;;;; *****************************

(defpackage #:app-config (:export #:*base-dir*))
(defparameter app-config:*base-dir*
  (make-pathname :name nil :type nil :defaults *load-truename*))

;;;; *********
;;;; Run Tests
;;;; *********

(defmethod perform ((o test-op) (c (eql (find-system :sudoku))))
  (operate 'load-op :sudoku-test)
  (funcall (intern (symbol-name :run-all-tests) 
                   (find-package :sudoku-test))))
