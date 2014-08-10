(in-package #:sudoku)

;;; **************** 
;;; Solving a puzzle
;;; **************** 

(defun choices (board index)
  "Find the possible choices for a given square at index." 
  (let ((vals (set-difference 
	        *choices*
                (let ((acc)
                      (peers (gethash index *friends*)))
                  (dotimes (n 20)
                    (let ((val (aref board (aref peers n))))
                      (if (not (zerop val))
                          (push val acc))))
                  (remove-duplicates acc)))))
    (if (= 1 (length vals))
        (car vals)
        (sort vals #'<))))

(defun initialize (board)
  "For the input board, find the possible chocies for every unfilled
   square and update the board to propagate the value in every square
   that is single valued." 
  (let ((newboard (cl-utilities:copy-array board)))
    (mapcar #'(lambda (i)
                (let ((val (aref board i)))
                  (if (= val 0)
                      (setf (aref newboard i)
                            (choices board i)))))
            *1d-indices*)
    newboard))

(defun friends-values (board ind)
  "Find the values of the friends of a square at index ind."
  (let ((acc)
        (peers (gethash ind *friends*)))
    (dotimes (n 20)
      (let ((val (aref board (aref peers n))))
        (if (numberp val)
            (push val acc))))
    acc))

(defun contradictions? (board)
  "A board has contradictions if two squares which are friends
   share the same value."
  (let ((count 0))
    (mapcar #'(lambda (ind)
                (let ((val (aref board ind)))
                  (if (numberp val)
                      (if (member val (friends-values board ind))
                          (return-from contradictions? 
				       (values t (incf count)))
                          (incf count)))))
            *1d-indices*)
    (values nil count)))

(defun solved? (board)
  "A board is solved if it has no contradictions.!"
  (multiple-value-bind (found count)
                       (contradictions? board)
    (and (= count 81) (not found))))

(defun update-friends (board val ind)
  "Eliminate the value from the friends of the square at index ind."
  (let ((peers (gethash ind *friends*)))
    (dotimes (n 20)
      (let ((vals (aref board (aref peers n))))
        (if (listp vals)
          (let ((newvals (remove val vals)))
            (setf (aref board (aref peers n))
                  (if (= 1 (length newvals))
                      (car newvals)
                      newvals))))))
    board))

(defun min-choice (board)
  "Find the square with minimum possible choices. There could be more
   than one such square."
  (let* ((choices (sort (filter #'(lambda (ind)
                          (let ((val (aref board ind)))
                            (if (and (listp val)
                                     (> (length val) 1))
                                (cons ind val))))
                      *1d-indices*)
                 #'(lambda (x y)
                     (< (length x) (length y)))
                 :key #'cdr)))
  (caar choices)))

(defun try (board)
  "Tries to solve the board by picking a square with minimum
   choices and applies each value in succession till one of
   the values results in a solution."
  (let ((newboard (update-board board)))
    (cond  ((null newboard) nil)	 ; contradiction was found
           ((solved? newboard) newboard) ; board is solved
           ;depth first search to find a solution
           (t (let ((min (min-choice newboard))) 
                (let ((vals (aref newboard min)))
                  (setf (aref newboard min)
                        (car vals))
                  (let ((valid (try newboard)))
                    (let ((nextvals (cdr vals)))
                      (if valid
                          (try valid)
                          (progn
			    (if (= (length nextvals) 1)
                                (setf (aref newboard min) 
                                      (car nextvals))
                                (setf (aref newboard min)
           		   	      nextvals))
                            (try newboard)))))))))))

(defun equal-boards (b1 b2)
  "Finds if two sudoku boards are equal."
  (mapcar #'(lambda (ind)
              (if (not (equal (aref b1 ind)
                              (aref b2 ind)))
                (return-from equal-boards nil)))
          *1d-indices*)
  t)

(defun update-board (board)
  "Eliminates the value in every single valued square from its
   friends till no more eliminations are possible or a contradiction
   is found."
  (let ((newboard (cl-utilities:copy-array board)))
   (dolist (ind *1d-indices*)
     (let ((val (aref board ind)))
       (when (numberp val)
           (setf newboard (update-friends newboard val ind)))))
   (unless (contradictions? newboard)
       (if (equal-boards board newboard)
           (return-from update-board board)
           (update-board newboard)))))

(defun solve (board)
  "The entry point function to solve a sudoku."
  (try (initialize board)))

;;; ********************************************************
;;; Solving puzzles stored in files, in grid or line formats
;;; ********************************************************

(defun solve-board-in-grid-format (grid-board)
  (solve (grid->board grid-board)))

(defun solve-board-in-line-format (line-board)
  (solve (line->board line-board)))

(defun solve-boards (board-lines board-fn filename board-type line-num-fn)
  (let ((i 0)
        (boards (mapcar board-fn board-lines))
        (total-time 0)
        (nil-solutions)
        (board-times))
    (format t "~%********************************************************")
    (format t "~% Solving ~A puzzles from file ~A" board-type filename)
    (format t "~%********************************************************~%")
    (dolist (board boards)
      (let* ((start (get-internal-real-time))
             (solution (solve board))
             (end (get-internal-real-time))
             (time-to-solve (/ (- end start) 1000.0)))
        (format t "Solved puzzle ~A (at line no ~A in file ~A) in ~,2F secs ~%" 
                (+ i 1) 
                (funcall line-num-fn i) 
                filename
                time-to-solve) 
        (unless solution
          (push (+ i 1) nil-solutions))
        (print-board solution)
        (format t "~%")
        (push time-to-solve board-times)
        (incf i)
        (incf total-time time-to-solve)))
    (format t "--------------------------------------------------~%")
    (if nil-solutions
      (format t " OOPS!!! These sudokus seem to have no solutions ~A %" 
              (reverse nil-solutions)))
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
                  board-type
                  #'(lambda (i)
                      (+ (* i 10) 1)))))

(defun solve-boards-in-line-format (file board-type)
  (solve-boards (file-lines file) 
                #'line->board
                (namestring file)
                board-type
                #'(lambda (i) 
                    (+ i 1))))

(defun print-board (board)
  (let ((2d-board (group (coerce board 'list)
                         9))
        (r 0))
    (dolist (row 2d-board)
      (if (or (= r 3)
              (= r 6))
        (format t "- - - - - - - - - - -~%"))
      (let ((sub-rows (group row 3)))
        (format t "~{~a~^ ~}" (nth 0 sub-rows))
        (format t " | ")
        (format t "~{~a~^ ~}" (nth 1 sub-rows))
        (format t " | ")
        (format t "~{~a~^ ~}~%" (nth 2 sub-rows)))
      (incf r))))

#+sbcl
(defun profile-solver ()
  (sb-profile:profile update-board try update-friends solve 
  contradictions?  solved? friends-values initialize choices
  min-choice use-discarded))
;;; "cl-sudoku" goes here. Hacks and glory await!

