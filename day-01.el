;; -*- aoc-date: (2024 1); -*-

(require 'advent-of-code)

;; part A
(let* ((data (aoc-read-input aoc-date))
       (left (seq-sort '< (mapcar (lambda (line)
				    (string-to-number (car (split-string line "\s" t " "))))
				  data)))
       (right (seq-sort '< (mapcar (lambda (line)
				     (string-to-number (cadr (split-string line "\s" t " ")))
				    )
				   data)))
       (sum 0))
  (while (car left)
    (let ((a (car left))
	  (b (car right)))
      (setq left (cdr left)
	    right (cdr right)
	    sum (+ sum (abs (- a b))))))
  (aoc-answer sum))


;; Part B
(let* ((data (aoc-read-input aoc-date))
       (left (seq-sort '< (mapcar (lambda (line)
				    (string-to-number (car (split-string line "\s" t " "))))
				  data)))
       (right (seq-sort '< (mapcar (lambda (line)
				     (string-to-number (cadr (split-string line "\s" t " ")))
				    )
				   data)))
       (answer (reduce '+ (mapcar (lambda (a)
				    (* a (length (seq-filter (lambda (b)
							       (= a b))
							     right))))
				  left))))
  (aoc-answer answer))


	    
