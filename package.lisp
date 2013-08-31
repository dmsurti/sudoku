;;;; package.lisp

(in-package #:cl-user)

(defpackage #:sudoku
  (:use #:cl)
  (:export #:solve 
           #:file-lines))

