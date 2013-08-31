(in-package #:sudoku)

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
