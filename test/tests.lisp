(in-package #:sudoku-test)

;;; *********************************** 
;;; Converting grid to universal format
;;; *********************************** 

(defun collect-grids (grid-lines)
  (labels ((collect-rec (lines &optional acc)
             (if (null lines) (nreverse acc)
                 (collect-rec (subseq lines 10)
                              (push (subseq lines 1 10) 
                                    acc)))))
    (collect-rec grid-lines)))

(defun multi-line->single-line (grid)
  (apply #'concatenate 'string grid))

(defun dot->zero (grid)
  (substitute #\0 #\. grid))

(defun grid->universal-format (grid)
  (dot->zero (multi-line->single-line grid)))

;;; *********************************** 
;;; Converting grid to universal format
;;; *********************************** 

(defun line->universal-format (line)
  (dot->zero line))

;;; *************************************************** 
;;; Converting universal format to a board, an array
;;; *************************************************** 

(defun universal-format->numeric-board (board)
  (mapcar #'(lambda (c)
              (parse-integer (string c)))
          (coerce board 'list)))

(defun numeric-board->board (board)
  (make-array 81 :initial-contents board))

(defun universal-format->board (board)
  (numeric-board->board 
    (universal-format->numeric-board board)))

;;; *****************************************   
;;; Converting a grid to a board, an array
;;; *****************************************   

(defun grid->board (grid)
  (universal-format->board
    (grid->universal-format grid)))

;;; *****************************************   
;;; Converting a grid to a board, an array
;;; *****************************************   

(defun line->board (line)
  (universal-format->board
    (line->universal-format line)))

;;; ********************************************************
;;; Solving puzzles stored in files, in grid or line formats
;;; ********************************************************

(defun solve-boards (board-lines board-fn filename board-type)
  (let ((i 0)
        (boards (mapcar board-fn board-lines))
        (total-time 0)
        (board-times))
    (format t "~%********************************************************")
    (format t "~% Solving ~A puzzles from file ~A" board-type filename)
    (format t "~%********************************************************~%")
    (dolist (board boards)
      (let* ((start (get-internal-real-time))
             (solution (solve board))
             (end (get-internal-real-time))
             (time-to-solve (/ (- end start) 1000.0)))
        (format t "Solved puzzle ~A in ~,2F secs ~%" 
                (+ i 1) time-to-solve) 
        (push time-to-solve board-times)
        (incf i)
        (incf total-time time-to-solve)))
    (format t "--------------------------------------------------~%")
    (format t "Solved ~A puzzles in ~,2F secs (avg ~,2F secs max ~,2F secs) ~%"
            i
            total-time
            (/ total-time i)
            (car (sort board-times #'>)))
    (format t "--------------------------------------------------~%")))

(defun solve-boards-in-grid-format (file board-type)
  (let ((grid-lines (file-lines file)))
    (solve-boards (collect-grids grid-lines) 
                  #'grid->board
                  (namestring file)
                  board-type)))

(defun solve-boards-in-line-format (file board-type)
  (solve-boards (file-lines file) 
                #'line->board
                (namestring file)
                board-type))

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
