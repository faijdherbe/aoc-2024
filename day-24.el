;; -*- aoc-date: (2024 24); -*-

(require 'advent-of-code)

(defun aoc-parse-input (input)
  (let (initial commands)
    (while (not (string= "" (car input)))
      (let* ((parts (string-split (car input) ":" t " "))
	     (line (car parts))
	     (value (if (string= "1" (cadr parts)) 1 0)))
;	(debug (car input) parts line value)

       (setq initial (plist-put initial line value))
       (setq input (cdr input))))

    (setq input (cdr input))

    (while (and (car input)
		(not (string= "" (car input))))
      (let* ((parts (string-split (car input) " " t " "))
	     (a (car parts))
	     (op (cadr parts))
	     (b (caddr parts))
	     (dst (nth 4 parts)))
	(push (list a b op dst) commands))
      (setq input (cdr input)))
    
    (list (cons 'lines initial)
	  (cons 'commands (nreverse commands)))))

(let* ((data (aoc-parse-input (aoc-read-input)))
       (wires (alist-get 'lines data))
       (commands (alist-get 'commands data)))
  (while commands
    (message "%d:%d" (length commands) (length wires))
    (dolist (c commands)
      (let ((a (plist-get wires (car c) 'string=))
	    (b (plist-get wires (cadr c) 'string=)))
	(when (and (identity a)
		   (identity b))
	  (let ((op (caddr c))
		(dst (cadddr c)))
	    (setq wires
		  (plist-put
		   wires
		   dst
		   (if 
		       (cond ((string= "XOR" op)
			      (not (= a b)))
			     ((string= "OR" op)
			      (or (= 1 a)
				  (= 1 b)))
			     ((string= "AND" op)
			      (and (= 1 a b))))
		       1 0)
		   'string=)))
	  (setq commands (remove c commands))))))
  (let ((out 0)
	(wires wires))
    (while wires
      (when (and (= ?z (elt (car wires) 0))
		 (= 1 (cadr wires)))
	(let ((i (string-to-number (substring (car wires) 1))))
	  (setq out (logior out (ash 1 i)))))
      (setq wires (cddr wires)))
    (aoc-answer out)))



