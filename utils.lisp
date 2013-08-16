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

(defconstant indexes '(0 1 2 3 4 5 6 7 8)
  "The rows and column indexes of the sudoku board.")

(defconstant boxes '((0 1 2) (3 4 5) (6 7 8))
  "The indexes for the boxes of the sudoku board.")

(defconstant units (apply #'append
			  (cross-product indexes indexes)
			  (cross-product indexes indexes #'(lambda (x y) 
							     (list y x)))
			  (cross-product boxes boxes 
					 #'(lambda (x y) 
					     (apply #'append 
						    (cross-product x y)))))
  "The 9 rows, 9 columns and 9 boxes of the sudoku.")

(defconstant indices (apply #'append (cross-product indexes indexes))
  "The indices from (0 0) to (8 8) of the 81 squares of the sudoku.")

(defconstant friends 
  (let ((lookup (make-hash-table :size 81 :test #'equal)))
    (mapcar #'(lambda (ind)
		(setf (gethash ind lookup) 
		      (remove-duplicates
		       (mapcan #'(lambda (u)
				   (if (member ind u :test #'equal)
				       (remove ind u :test #'equal)))
			       units)
		       :test #'equal)))
	    indices)
    lookup)
  "The friends of a given square looked up by the square indices. Every
   square has 20 friends.")

(defconstant choices '(1 2 3 4 5 6 7 8 9)
  "All possible valid choices for every unfilled square of the sudoku.")
