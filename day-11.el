;; -*- aoc-date: (2024 11); -*-

(require 'advent-of-code)


(defun aoc-blink (stone)
  (cond ((= 0 stone)
	 (list 1))
	((= 0 (% (length (number-to-string stone)) 2))
	 (let* ((str (number-to-string stone))
		(l (/ (length str) 2)))
	   (mapcar 'string-to-number
		   (list (substring str 0 l)
			 (substring str l)))))
	(t
	 (list (* 2024 stone)))))

(defun aoc-process (stone iterations)
  (let ((stones (aoc-blink stone)))
    (if (> iterations 1)
	(apply '+ (mapcar (lambda (st)
			    (aoc-process st (1- iterations)))
			  stones))
      (length stones))))

(defun aoc-blink-stones (input count)
   (apply '+ (mapcar (lambda (st)
		       (aoc-process st count))
		     input)))  

;; Part A
(aoc-answer
 (aoc-blink-stones
  (mapcar 'string-to-number
	  (aoc-read-input nil 'string-split))
  25))
