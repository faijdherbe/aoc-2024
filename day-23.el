;; -*- aoc-date: (2024 23); -*-

(require 'advent-of-code)

(defun aoc-register-pair (pair table)
  (puthash (car pair)
	   (add-to-list 
	    (gethash (car pair)
		     table
		     nil)
		  (cdr pair))
	   table)
  (puthash (cdr pair)
	   (add-to-list
	    (gethash (cdr pair)
		     table
		     nil)
	    (car pair))
	   table))

(let* ((table (make-hash-table :test 'equal))
       (input (aoc-read-input))
       (input (mapcar (lambda (l)
			(let ((ll (string-split l "-")))
			  (cons (car ll)
				(cadr ll))))
		      input))
       (input (mapcar (lambda (x)
			(aoc-register-pair x table)
			x)
		      input)))
  (let ((candidates (seq-filter (lambda (c)
				  (= ?t (elt c 0)))
				(hash-table-keys table))))
    (aoc-answer
     (length
      (seq-filter (lambda (n)
		    (= 2 (length (seq-uniq (gethash n table) ))))
		  candidates)))))
