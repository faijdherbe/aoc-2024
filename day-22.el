;; -*- aoc-date: (2024 22); -*-

(require 'advent-of-code)

;;* Calculate the result of multiplying the secret number by 64.
;; Then, mix this result into the secret number. Finally, prune
;; the secret number.
;;* Calculate the result of dividing the secret number by 32.
;; Round the result down to the nearest integer. Then, mix this
;; result into the secret number. Finally, prune the secret
;; number.
;;* Calculate the result of multiplying the secret number by
;; 2048. Then, mix this result into the secret number. Finally,
;; prune the secret number.
;;
;;Each step of the above process involves mixing and pruning:
;;
;;* To mix a value into the secret number, calculate the bitwise
;; XOR of the given value and the secret number. Then, the
;; secret number becomes the result of that operation. (If the
;; secret number is 42 and you were to mix 15 into the secret
;; number, the secret number would become 37.)
;;* To prune the secret number, calculate the value of the
;; secret number modulo 16777216. Then, the secret number
;; becomes the result of that operation. (If the secret number
;; is 100000000 and you were to prune the secret number, the
;; secret number would become 16113920.)
;;

(defun aoc-mix-prune (x s)
  (mod (logxor x s) 16777216))

(defun aoc-a (x)
  (aoc-mix-prune (* 64 x) x))

(defun aoc-b (x)
  (aoc-mix-prune (/ x 32) x))

(defun aoc-c (x)
  (aoc-mix-prune (* 2048 x) x))

;; Part A
(aoc-answer
 (apply '+
	(mapcar (lambda (x)
		  (dotimes (i 2000)
		    (setq x (aoc-a x))
		    (setq x (aoc-b x))
		    (setq x (aoc-c x)))
		  x)
		  
		(mapcar 'string-to-number
			(aoc-read-input)))))
