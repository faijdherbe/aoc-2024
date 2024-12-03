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

(defun aoc-normalize-line (line)
  (mapcar 'string-to-number (string-split line " " t " ")))

(defun aoc-report-valid-p (data)
  (and (apply 'max-3-diff-p data)
       (or (apply '> data)
	   (apply '< data))))

(aoc-answer 
 (let ((data (aoc-read-input aoc-date)))
   (length (seq-filter 'aoc-report-valid-p
		       (mapcar 'aoc-normalize-line data)))))


;; Part B
(defun aoc-report-valid-dampened-p (line &optional i)
    (let ((i (or i 0)))
    (or (aoc-report-valid-p (seq-remove-at-position line i))
	(and (< (1+ i) (length line))
	     (aoc-report-valid-dampened-p line (+ 1 i))))))

(aoc-answer 
 (let ((data (aoc-read-input aoc-date)))
   (length (seq-filter 'aoc-report-valid-dampened-p
		       (mapcar 'aoc-normalize-line data)))))

