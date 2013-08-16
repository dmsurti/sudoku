;;;; sudoku.lisp

(in-package #:sudoku)

(defun choices (board i j)
  "Find the possible choices for a given square at (i j)." 
  (let ((vals (set-difference 
	        choices
                (remove-duplicates
                  (mapcar #'(lambda (ind)
                              (let ((val (apply #'aref board ind)))
                                (if (not (zerop val))
                                    val)))
                          (gethash (list i j) friends))))))
    (if (= 1 (length vals))
        (car vals)
        (sort vals #'<))))

(defun initialize (board)
  "For the input board, find the possible chocies for every unfilled
   square and update the board to propagate the value in every square
   that is single valued." 
  (let ((newboard (cl-utilities:copy-array board)))
    (mapcar #'(lambda (i)
                (let ((val (apply #'aref board i)))
                  (if (= val 0)
                      (setf (apply #'aref newboard i)
                            (apply #'choices board i)))))
          indices)
    (update-board newboard)))

(defun friends-values (board ind)
  "Find the values of the friends of a square at index ind."
  (filter #'(lambda (in)
              (let ((val (apply #'aref board in)))
                   (if (numberp val)
                       val)))
          (gethash ind friends)))

(defun contradictions? (board)
  "A board has contradictions if two squares which are friends
   share the same value."
  (let ((count 0))
    (mapcar #'(lambda (ind)
                (let ((val (apply #'aref board ind)))
                  (if (numberp val)
                      (if (member val (friends-values board ind))
                          (return-from contradictions? 
				       (values t (incf count)))
                          (incf count)))))
            indices)
    (values nil count)))

(defun solved? (board)
  "A board is solved if it has no contradictions.!"
  (multiple-value-bind (found count)
                       (contradictions? board)
   (if (and (= count 81) (not found))
       t
       nil)))

(defun update-friends (board val ind)
  "Eliminate the value from the friends of the square at index ind."
  (mapcar #'(lambda (in)
              (let ((vals (apply #'aref board in)))
                (if (listp vals)
                    (let ((newvals (remove val vals)))
                      (setf (apply #'aref board in)
                            (if (= 1 (length newvals))
                                (car newvals)
                                newvals))))))
         (gethash ind friends))
  board)

(defun min-choice (board)
  "Find the square with minimum possible choices. There could be more
   than one such square."
  (caar (sort (filter #'(lambda (ind)
                        (let ((val (apply #'aref board ind)))
                          (if (and (listp val)
                                   (>= (length val) 2))
                              (cons ind val))))
              indices)
              #'(lambda (x y)
                  (< (length x) (length y)))
             :key #'cdr)))

(defun try (board)
  "Tries to solve the board by picking a square with minimum
   choices and applies each value in succession till one of
   the values results in a solution."
  (let ((newboard (update-board board)))
    (cond  ((null newboard) nil)	 ; contradiction was found
           ((solved? newboard) newboard) ; board is solved 
           (t (let ((min (min-choice newboard))) ;depth first search to find a solution
                (let ((vals (apply #'aref newboard min)))
                  (setf (apply #'aref newboard min)
                        (car vals))
                  (let ((valid (try newboard)))
                    (let ((nextvals (cdr vals)))
                      (if valid
                          (progn
                            (dolist (val nextvals)
			      (setf valid (use-discarded val valid min))
                              (if (null valid) nil))
                            (try valid))
                          (progn
			    (if (= (length nextvals) 1)
                                (setf (apply #'aref newboard min) 
                                      (car nextvals))
                                (setf (apply #'aref newboard min)
           		   	      nextvals))
                            (setf newboard (use-discarded (car vals)
                                                          newboard min))
                            (if (null newboard) nil)
                            (try newboard)))))))))))

(defun use-discarded (val board min)
  "Checks if a value discarded from a square at index min is
   usable in only of its friends. In that case it applies that
   discarded value to the friend. If none of the friends can
   use that discarded value implies a contradiction."
  (let ((count 0) ind)
    (dolist (f (gethash min friends))
      (let ((vals (apply #'aref board f)))
        (if (and (listp vals) (find val vals))
            (progn
              (incf count)
              (setf ind f)))))
    (if (= count 0)
        nil)
    (if (= count 1)
        (setf (apply #'aref board ind) val)))
    board)

(defun equal-boards (b1 b2)
  "Finds if two sudoku boards are equal."
  (mapcar #'(lambda (ind)
              (if (not (equal (apply #'aref b1 ind)
                              (apply #'aref b2 ind)))
                (return-from equal-boards nil)))
          indices)
  t)

(defun update-board (board)
  "Eliminates the value in every single valued square from its
   friends till no more eliminations are possible or a contradiction
   is found."
  (let ((newboard (cl-utilities:copy-array board)))
   (dolist (ind indices)
     (let ((val (apply #'aref board ind)))
       (if (numberp val)
           (setf newboard (update-friends newboard val ind)))))
   (if (contradictions? board)
       nil
       (if (equal-boards board newboard)
           board
           (update-board newboard)))))

(defun solve (board)
  "The entry point function to solve a sudoku."
  (format t "~% The solution is ~A " (try (initialize board))))

;(sb-profile:profile update-board try update-friends solve 
;contradictions?  solved? friends friends-values initialize choices
;min-choice use-discarded)
;;; "cl-sudoku" goes here. Hacks and glory await!

