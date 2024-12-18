;; -*- aoc-date: (2024 18); -*-

(require 'advent-of-code)

(load-file "day-16.el")

(defun dijkstra--cost (d s ss)
  1)

(defun aoc-parse-map (input bounds)
  (let ((r '()))
    (dotimes (x (car bounds))
      (dotimes (y (cdr bounds))
	(unless (member (format "%d,%d" x y) input)
	  (setq r (cons (cons x y) r)))))
    r))


;; Part A
(aoc-answer
 (let* ((input (aoc-read-input))
	(input (seq-take input 1024))
	(bounds '(71 . 71))
	(map (aoc-parse-map input bounds))
	(start '(0 . 0))
	(exit (cons (1- (car bounds))
		    (1- (cdr bounds)))))
   (let-alist (dijkstra map start) 
     (gethash exit .dist))))
		
  
