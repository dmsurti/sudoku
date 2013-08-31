(in-package #:sudoku-test)

;;; **************************
;;; Interface to run all tests
;;; **************************

(defun run-all-tests ()
  (solve-boards-in-grid-format #p"test/easy.txt" 
                               "easy")
  (solve-boards-in-line-format #p"test/hard-top95.txt" 
                               "hard")
  (solve-boards-in-line-format #p"test/hardest.txt" 
                               "hardest"))
