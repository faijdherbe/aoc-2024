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


(defun aoc-calc-prices (secret iterations)
  (let ((x secret))
    (dotimes (i iterations)
      (setq x (aoc-a x))
      (setq x (aoc-b x))
      (setq x (aoc-c x)))
    x))
		  


;; Part A
(aoc-answer
 (apply '+
	(mapcar (lambda (x)
		  (aoc-calc-prices x 2000))
	    (mapcar 'string-to-number
		    (aoc-read-input)))))

;; Part B
(defun aoc-calc-prices (secret iterations)
  (let ((out '())
	(x secret))
    (dotimes (i iterations)
      (setq x (aoc-a x))
      (setq x (aoc-b x))
      (setq x (aoc-c x))
      (setq out (push x out)))
    out))

(defun aoc-calc-key (data)
  (let ((out '())
	(data data))
    (dotimes (i 4)
      (push (- (cadr data)
	       (car data))
	    out)
      (setq data (cdr data)))
    out))

(let* ((input (aoc-read-input))
       (input (mapcar 'string-to-number input))
       (input (mapcar (lambda (x)
		(aoc-calc-prices x 2000))
	      input))
       (input (mapcar (lambda (x)

			 (mapcar (lambda (x) (% x 10))
				 x))
		      input))
       (groups (make-hash-table :test 'equal)))
  (dolist (seller input)
    (dotimes (i (- (length seller) 4))
      (let* ((part (take 5 seller))
	     (key (aoc-calc-key part))
	     (v (car part)))
	
	(puthash key 	       
		 (+ v (gethash key groups 0))
		 groups)
      (setq seller (cdr seller)))))
  (aoc-answer (car
	       (seq-sort '> (hash-table-values groups)))))

