;; -*- aoc-date: (2024 14); -*-

(require 'advent-of-code)

(defun make-robot (px py vx vy)
  (list (cons 'p (list (cons 'x px)
		       (cons 'y py)))
	(cons 'v (list (cons 'x vx)
		       (cons 'y vy)))))

(defun string-to-robot (string)
  (let* ((q (split-string string " "))
	 (p (split-string (cadr (split-string (car q) "=")) ","))
	 (v (split-string (cadr (split-string (cadr q) "=")) ","))
	 (p (mapcar 'string-to-number p))
	 (v (mapcar 'string-to-number v)))
    (make-robot (car p) (cadr p) (car v) (cadr v))))

(defun move-robot (robot seconds max)
  (let-alist robot
    (let ((x (% (+ .p.x (* seconds .v.x) (* seconds 2 (car max))) (car max)))
	  (y (% (+ .p.y (* seconds .v.y) (* seconds 2 (cdr max))) (cdr max))))
      (make-robot x y .v.x .v.y))))

(aoc-answer (let-alist (seq-group-by (lambda (r)
		(let-alist r
		  (cond ((and (< .p.x 50) (< .p.y 51))
			 'a)
			((and (< .p.x 50) (> .p.y 51))
			 'b)
			((and (> .p.x 50) (< .p.y 51))
			 'c)
			((and (> .p.x 50) (> .p.y 51))
			 'd))))
	      
	      
	      (mapcar (lambda (r)
			(move-robot r 100 (cons 101 103)))
		      (mapcar 'string-to-robot
			      (aoc-read-input))))
  (* (length .a)
     (length .b)
     (length .c)
     (length .d))))
