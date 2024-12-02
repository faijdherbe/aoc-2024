;; -*- aoc-date: (2024 2); -*-

(require 'advent-of-code)

;; part A
(defun max-3-diff-p (&rest args)
  (let ((h 0))
    (reduce (lambda (c v)
		  (let ((d (abs (- c v))))
		    (when (< h d)
		      (setq h d))
		    v))
		args)
    (<= h 3)))

(defun aoc-report-valid-p (line)
  (let ((data (mapcar 'string-to-number (string-split line " " t " "))))
    (and (apply 'max-3-diff-p data)
	 (or (apply '> data)
	     (apply '< data)))))    

(aoc-answer 
 (let ((data (aoc-read-input aoc-date)))
   (length (seq-filter 'aoc-report-valid-p
		       data))))
       
