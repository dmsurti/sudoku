(in-package #:sudoku-test)

;;; **************
;;; Test Resources
;;; **************

(defvar *test-dir*
  (merge-pathnames "test/" app-config:*base-dir*))

(defvar *easy-tests* (merge-pathnames "easy.txt" *test-dir*))
(defvar *hard-tests* (merge-pathnames "hard-top95.txt" *test-dir*))
(defvar *hardest-tests* (merge-pathnames "hardest.txt" *test-dir*))

;;; **************************
;;; Interface to run all tests
;;; **************************

(defun run-all-tests ()
  (solve-boards-in-grid-format *easy-tests*
                               "easy")
  (solve-boards-in-line-format *hard-tests*
                               "hard")
  (solve-boards-in-line-format *hardest-tests*
                               "hardest"))
