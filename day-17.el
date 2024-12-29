;; -*- aoc-date: (2024 17); -*-

(require 'advent-of-code)


(defun aoc-init-mem (A B C pointer writer)
  (list 'A A
	'B B
	'C C
	'pointer pointer
	'writer writer))

(defun aoc--adv (op x mem)
  "The adv instruction (opcode 0) performs division. The
numerator is the value in the A register. The denominator is
found by raising 2 to the power of the instruction's combo
operand. (So, an operand of 2 would divide A by 4 (2^2); an
operand of 5 would divide A by 2^B.) The result of the
division operation is truncated to an integer and then written
to the A register."
  (plist-put mem
	     'A
	     (floor (/ (plist-get mem 'A)
		       (expt 2 (aoc-combo x mem))))))

(defun aoc--bxl (op x mem)
  "The bxl instruction (opcode 1) calculates the bitwise XOR of
register B and the instruction's literal operand, then stores
the result in register B."
  (plist-put mem
	     'B
	     (logxor (plist-get mem 'B) x)))

(defun aoc--bst (op x mem)
  "The bst instruction (opcode 2) calculates the value of its
combo operand modulo 8 (thereby keeping only its lowest 3
bits), then writes that value to the B register."
  (plist-put mem
	     'B
	     (mod (aoc-combo x mem)
		  8)))

(defun aoc--jnz (op x mem)
  "The jnz instruction (opcode 3) does nothing if the A register
is 0. However, if the A register is not zero, it jumps by
setting the instruction pointer to the value of its literal
operand; if this instruction jumps, the instruction pointer is
not increased by 2 after this instruction."
  (if (= 0 (plist-get mem 'A))
      mem
    (progn
      (plist-put mem
		 'pointer x))))

(defun aoc--bxc (op x mem)
  "The bxc instruction (opcode 4) calculates the bitwise XOR of
register B and register C, then stores the result in register
B. (For legacy reasons, this instruction reads an operand but
ignores it.)"
  (plist-put mem 'B
	     (logxor (plist-get mem 'B)
		     (plist-get mem 'C))))

(defun aoc--out (op x mem)
  "The out instruction (opcode 5) calculates the value of its
combo operand modulo 8, then outputs that value. (If a program
outputs multiple values, they are separated by commas.)"
  (apply (plist-get mem 'writer)
	 (list (mod (aoc-combo x mem) 8)))
  mem)

(defun aoc--bdv (op x mem)
  "The bdv instruction (opcode 6) works exactly like the adv
instruction except that the result is stored in the B
register. (The numerator is still read from the A register.)"
  (plist-put mem
	     'B
	     (floor (/ (plist-get mem 'A)
		       (expt 2 (aoc-combo x mem))))))

(defun aoc--cdv (op x mem)
  "The cdv instruction (opcode 7) works exactly like the adv
instruction except that the result is stored in the C
register. (The numerator is still read from the A register.)"
  (plist-put mem
	     'C
	     (floor (/ (plist-get mem 'A)
		       (expt 2 (aoc-combo x mem))))))

(setq aoc-ops (list
	       0 'aoc--adv
	       1 'aoc--bxl
	       2 'aoc--bst
	       3 'aoc--jnz
	       4 'aoc--bxc
	       5 'aoc--out
	       6 'aoc--bdv
	       7 'aoc--cdv))

(defun aoc--op (op x mem)
  (apply (plist-get aoc-ops op)
	 (list op x mem)))

(defun aoc-combo (x mem)
  (cond ((member x '(0 1 2 3))
	 x)
	((= 4 x)
	 (plist-get mem 'A))
	((= 5 x)
	 (plist-get mem 'B))
	((= 6 x)
	 (plist-get mem 'C))
	(t nil)))

(defun aoc-run-program (program mem)
	 (let ((pl (length program)))
  (while (< (plist-get mem 'pointer) pl)
    (let* ((p (plist-get mem 'pointer))
	   (op (nth p program))
	   (v (nth (1+ p) program)))
      (setq mem (plist-put mem 'pointer (+ 2 p)))
      (setq mem (aoc--op op v mem))))))  


(defun aoc-parse-program (input writer)
  (let (A B C (pointer 0) program)
    (dolist (line input)
      (let* ((p (string-split line ":" t " "))
	     (k (car p))
	     (v (cadr p)))
	(cond ((string= "Register A" k)
	       (setq A (string-to-number v)))
	      ((string= "Register B" k)
	       (setq B (string-to-number v)))
	      ((string= "Register C" k)
	       (setq C (string-to-number v)))
	      ((string= "Program" k)
	       (setq program (mapcar 'string-to-number
				     (string-split v ",")))))))
    (list (cons 'mem (aoc-init-mem A B C pointer writer))
	  (cons 'program program))))

(aoc-answer (let ((out '()))
  (let-alist (aoc-parse-program (aoc-read-input)
				(lambda (v)
				  (setq out (push v out))))
    (aoc-run-program .program .mem)
    (string-join (mapcar 'number-to-string
			 (nreverse out))
		 ","))))
