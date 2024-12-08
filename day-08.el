;; -*- aoc-date: (2024 8); -*-

(require 'advent-of-code)

(defun <> (c l u)
  (and (>= c l)
       (<= c u)))


(defun antennap (c)
  (or (<> c ?0 ?9)
      (<> c ?a ?z)
      (<> c ?A ?Z)))

(defun aoc-load-antennas (input)
  (let ((antennas (make-hash-table :test 'eq))
	(y 0)
	(h (length input))
	(w 0))
    (dolist (row input)
      (when (= 0 w)
	(setq w (length row)))
      (dotimes (x (length row))
	(let ((char (elt row x)))
	  (when (antennap char)
	    (push (cons x y) (gethash char antennas nil)))))
	  (setq y (1+ y)))
    `((antennas . ,antennas)
      (dim . (,w . ,h)))))

(defun aoc-translate (a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

(defun aoc-vect-diff (a b)
  (cons (- (car a) (car b))
	(- (cdr a) (cdr b))))

(defun aoc-vect-invert (a)
  (cons (* -1 (car a))
	(* -1 (cdr a))))

(defun aoc-antinodes (antennas)
  (let ((antinodes '()))
    (while (cdr antennas)
      (let ((a (car antennas)))
	(dolist (b (cdr antennas))
	  (let ((vect (aoc-vect-diff a b)))
	    (add-to-list 'antinodes (aoc-translate a vect))
	    (add-to-list 'antinodes (aoc-translate b (aoc-vect-invert vect)))))
	(setq antennas (cdr antennas))))
    antinodes))


(defun aoc-valid-p (l dim)
  (and (>= (car l) 0)
       (>= (cdr l) 0)
       (< (car l) (car dim))
       (< (cdr l) (cdr dim))))
  
;;; Part A
(let ((setup (aoc-load-antennas (aoc-read-input)))
      (nodes '()))
  (let-alist setup
    (dolist (n (hash-table-keys .antennas))
      (dolist (node (aoc-antinodes (gethash n .antennas)))
	(when (aoc-valid-p node .dim)
	  (add-to-list 'nodes node)))))
  (aoc-answer (length nodes)))
  

	     
