;; -*- aoc-date: (2024 5); -*-

(require 'advent-of-code)

;;; Part A
(defun aoc-parse-d5 (input)
  (let ((parts (split-string input "\n\n" t)))
    `((rules . ,(split-string (car parts) "\n" t))
      (updates . ,(split-string (cadr parts) "\n" t)))))

(defun aoc-parse-rules (rules)
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (rule (mapcar (lambda (rule) (string-split rule "|")) rules))
      (push (cadr rule) (gethash (car rule) groups nil)))
    groups))

(defun aoc-parse-updates (updates)
  (mapcar (lambda (upd)
	    (split-string upd "," t))
	    updates))

(defun aoc-filter-updates (updates rules &optional inverse)
  (seq-filter (lambda (update)
		(let* ((update (reverse update))
		       (ok t)
		       (k (car update)))
		  (while (and ok k)
		    (setq ok 
			  (= 0 (length (seq-intersection (gethash k rules)
							 (cdr update)))))
		    (setq update (cdr update))
		    (setq k (car update)))
		  (if inverse (not ok)
		    ok)))
	      updates))

(defun aoc-take-middle (seq)
  (nth (ceiling (/ (length seq) 2)) seq))

(aoc-answer
 (let-alist (aoc-read-input nil 'aoc-parse-d5)
   (let* ((rules (aoc-parse-rules .rules))
	  (updates (aoc-parse-updates .updates))
	  (valid-updates (aoc-filter-updates updates rules))
	  (middles (mapcar 'aoc-take-middle valid-updates))
	  (middles (mapcar 'string-to-number middles)))
     (apply '+ middles))))

;;; Part B

(defun aoc-sort-update (update rules)
  (sort update (lambda (a b)
	  (member b (gethash a rules)))))

(aoc-answer
 (let-alist (aoc-read-input nil 'aoc-parse-d5)
   (let* ((rules (aoc-parse-rules .rules))
	  (updates (aoc-parse-updates .updates))
	  (updates (aoc-filter-updates updates rules t))
	  (updates (mapcar (lambda (update)
			     (aoc-sort-update update rules))
			   updates))
	  (middles (mapcar 'aoc-take-middle updates))
	  (middles (mapcar 'string-to-number middles)))
     (apply '+ middles))))


