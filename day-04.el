;; -*- aoc-date: (2024 4); -*-

(require 'advent-of-code)

(defun aoc-build-map (input)
  `((map . ,input)
    (dim . ((w . ,(length (nth 0 input)))
	    (h . ,(length input))))))


(defun aoc-char-from-map (map loc)
  (or (let-alist map
    (let ((x (car loc))
	  (y (cdr loc)))
      (when (and (> .dim.w x)
		 (>= x 0)
		 (> .dim.h y)
		 (>= y 0))
	(elt (nth y .map) x))))
      0))

(defun aoc-translate (loc dir)
  (cons (+ (car loc) (car dir))
	(+ (cdr loc) (cdr dir))))

(defun aoc-read-word (map loc dir len)
  (let ((c (aoc-char-from-map map loc)))
    (if (> (1- len) 0)
	(cons c (aoc-read-word map
			       (aoc-translate loc dir)
			       dir
			       (1- len)))
      (cons c nil))))

(defun aoc-find-occurances (word map)
  (let ((result '()))
    (let-alist map
      (dotimes (y .dim.h)
	(dotimes (x .dim.w)
	  (when (eq ?X (aoc-char-from-map map (cons x y)))
	    (dolist (dir '((1 . 0) (1 . 1) (0 . 1) (-1 . 1) (-1 . 0) (-1 . -1) (0 . -1) (1 . -1)))
	      (when (string= word (concat (aoc-read-word map
						 (cons x y)
						 dir
						 (length word))))
		(add-to-list 'result `((loc . ,(cons x y))
				       (dir . ,dir)))))))))
    result))




;;; Part A
(aoc-answer 
 (length
  (aoc-find-occurances
   "XMAS"
   (aoc-build-map (aoc-read-input)))))

;;; Part B

(defun aoc-has-xmas (map loc)
  (and (or (and (eq ?M (aoc-char-from-map map (aoc-translate loc (cons -1 1))))
		(eq ?S (aoc-char-from-map map (aoc-translate loc (cons 1 -1)))))
	   (and (eq ?S (aoc-char-from-map map (aoc-translate loc (cons -1 1))))
		(eq ?M (aoc-char-from-map map (aoc-translate loc (cons 1 -1))))))
       
       (or (and (eq ?M (aoc-char-from-map map (aoc-translate loc (cons 1 1))))
		(eq ?S (aoc-char-from-map map (aoc-translate loc (cons -1 -1)))))
	   (and (eq ?S (aoc-char-from-map map (aoc-translate loc (cons 1 1))))
		(eq ?M (aoc-char-from-map map (aoc-translate loc (cons -1 -1))))))))

	       

(defun aoc-find-occurances-xmas (map)
  (let ((result '()))
    (let-alist map
      (dotimes (y .dim.h)
	(dotimes (x .dim.w)
	  (when (eq ?A (aoc-char-from-map map (cons x y)))
	    (when (aoc-has-xmas map (cons x y))
	      (add-to-list 'result `((loc . ,(cons x y)))))))))
				     
    result))

(aoc-answer 
 (length
  (aoc-find-occurances-xmas
   (aoc-build-map (aoc-read-input)))))

