(in-package #:sudoku)

(defun filter (fn lst)
  "Filters the list according to the predicate."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cross-product (lst1 lst2 &optional (fn #'list))
    "Finds the cross-product between the elements of two lists."
    (mapcar #'(lambda (x)
		(mapcar #'(lambda (y)
			    (funcall fn x y))
			lst2))
	    lst1)))

(defparameter *indexes* '(0 1 2 3 4 5 6 7 8)
  "The rows and column indexes of the sudoku board.")

(defparameter *boxes* '((0 1 2) (3 4 5) (6 7 8))
  "The indexes for the boxes of the sudoku board.")

(defparameter *units* (apply #'append
			  (cross-product *indexes* *indexes*)
			  (cross-product *indexes* *indexes* #'(lambda (x y) 
							     (list y x)))
			  (cross-product *boxes* *boxes* 
					 #'(lambda (x y) 
					     (apply #'append 
						    (cross-product x y)))))
  "The 9 rows, 9 columns and 9 boxes of the sudoku.")



(defparameter *indices* (apply #'append (cross-product *indexes* *indexes*))
  "The indices from (0 0) to (8 8) of the 81 squares of the sudoku.")

(defparameter *1d-indices* 
  (mapcar #'(lambda (index)
              (+ (* (car index) 9)
                 (cadr index)))
          *indices*)
  "The zero-indexed indices from 0 to 80 of the 81 squares of the sudoku.")

(defparameter *square-units*
  (let ((lookup (make-hash-table :size 81 :test #'equal)))
    (mapcar #'(lambda (ind)
		(setf (gethash (+ (* (car ind) 9)
                                  (cadr ind))
                               lookup) 
                      (let ((units (mapcar #'(lambda (unit)
                                               (if (member ind
                                                           unit
                                                           :test #'equal)
                                                 unit))
                                           *units*)))
                        (remove-duplicates
                          (filter #'(lambda (index)
                                      (unless (equal index ind)
                                        (+ (* (car index) 9)
                                              (cadr index))))
                                  (apply #'append units))))))
            *indices*)
    lookup)
  "The units that each square belongs to")


(defparameter *friends* 
  (let ((lookup (make-hash-table :size 81 :test #'equal)))
    (mapcar #'(lambda (ind)
		(setf (gethash (+ (* (car ind) 9)
                                  (cadr ind))
                               lookup) 
		      (let ((peers (remove-duplicates
                                       (mapcan #'(lambda (u)
                                                   (if (member ind 
                                                               u 
                                                               :test #'equal)
                                                       (remove ind 
                                                               u 
                                                               :test #'equal)))
                                                *units*)
                                       :test #'equal)))
                        (make-array 20 
                                    :initial-contents 
                                    (mapcar #'(lambda (peer) 
                                                (+ (* (car peer) 9)
                                                   (cadr peer)))
                                            peers)))))
	    *indices*)
    lookup)
  "The friends of a given square looked up by the square indices. Every
   square has 20 friends.")

(defparameter *choices* '(1 2 3 4 5 6 7 8 9)
  "All possible valid choices for every unfilled square of the sudoku.")

;;; ********************
;;; General Utility code
;;; ********************

(defun file-lines (file)
  "Returns the lines in a file as a list."
  (let ((all-lines))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil :eof)
                 (read-line str nil :eof)))
          ((eql line :eof))
           (push line all-lines)))
    (nreverse all-lines)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
                (if (consp rest)
                  (rec rest (cons (subseq source 0 n) acc))
                  (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

 (defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))
