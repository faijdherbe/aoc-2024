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




;; part b
(defun aoc-find-exit (map start exit)
  (let-alist (dijkstra map start) 
    ((gethash exit .dist)))

(let* ((input (aoc-read-input))
       (bounds '(71 . 71))
       (start '(0 . 0))		 
       (exit (cons (1- (car bounds))
		   (1- (cdr bounds))))
       (op '+)
       (offset 1204)
       (stepsize (round (/ (- (length input) offset) 2)))
       (d 0))
  (while (> stepsize 0)
    (message "offset: %d(%d), op: %S, stepsize: %d, d: %d" offset (length input) op stepsize d)
    (let ((dist (aoc-find-exit (aoc-parse-map (seq-take input offset) bounds) start exit)))
      (when dist
	(setq d dist))

      (when (or (and dist
		     (eq '- op))
		(and (not dist)
		     (eq '+ op)))
	(setq stepsize (round (/ stepsize 2)))
	(setq op (if (eq '+ op)
		     '- '+)))
      
      (setq offset (apply op (list offset stepsize)))))
  (aoc-answer (nth (1- offset) input)))

  
 
  
