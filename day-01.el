;; -*- advent-of-code-date: (2024 1); -*-

;; move to util class
(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

;; part A
(let* ((data (read-lines "input/01.txt"))
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
  (message "part A: %d" sum))


;; Part B
(let* ((data (read-lines "input/01.txt"))
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
  (message "answer B: %d" answer))


	    
