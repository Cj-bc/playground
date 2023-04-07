;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;
;; This file contains some code snippets that are from:
;; https://gigamonkeys.com/book/practical-a-simple-database.html
;;
;; Even though I, Cj-bc, am writing this by my hand, all codes are based on one on the site.


(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))


(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))


(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd (prompt-read "Title")
	   (prompt-read "artist")
	   (or (parse-integer (prompt-read "rating") :junk-allowed t) 0)
	   (y-or-n-p "Is ripped? [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
	(if (not (y-or-n-p "Add more?[Y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (setf *db* (read in))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
	collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest fields)
  `#'(lambda (cd) (and ,@(make-comparisons-list fields))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db* (mapcar #'(lambda (cd)
	      (when (funcall selector-fn cd)
		(if title (setf (getf cd :title) title))
		(if artist (setf (getf cd :artist) artist))
		(if rating (setf (getf cd :rating) rating))
		(if ripped-p (setf (getf cd :riped) ripped)))
	      cd) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
