;; -*- aoc-date: (2024 12); -*-

(require 'advent-of-code)

(defun aoc-i-xy (i w h)
  (cons (% i w) (floor (/ i w))))

(defun aoc-c (map i)
  (elt map i))

(defun aoc-make-group (id nodes)
  `((id . ,id)
    (nodes . ,nodes)))

(defun aoc-scan-group (i map w h)
  (let ((c (aoc-c map i))
	(seen nil)
	(todo (list i))
	(nodes nil)
	(m (length map)))
    (while todo
      (let ((node (pop todo)))
	(push node nodes)

	(let ((pp (- node w)))
	  (when (and (>= pp 0)
		     (not (member pp seen))
		     (= c (aoc-c map pp)))
	    (push pp todo))
	  (add-to-list 'seen pp))

	(let ((pp (+ node w)))
	  (when (and (< pp m)
		     (not (member pp seen))
		     (= c (aoc-c map pp)))
	    (push pp todo))
	  (add-to-list 'seen pp))

	(let ((pp (- node 1)))
	  (when (and
		 (>= pp 0)
		 (= (floor (/ node w))
		    (floor (/ pp w)))
		 (not (member pp seen))
		 (= c (aoc-c map pp)))
	    (push pp todo))
	  (add-to-list 'seen pp))

	(let ((pp (+ node 1)))
	  (when (and
		 (< pp m)
		 (= (floor (/ node w))
		    (floor (/ pp w)))
		 (not (member pp seen))
		 (= c (aoc-c map pp)))
	    (push pp todo))
	  (add-to-list 'seen pp))))
    nodes))

(defun aoc-2024-12a (input)
  (let ((map (string-join input))
	(h (length input))
	(w (length (nth 0 input)))
	(seen nil)
	(groups (make-hash-table :test 'equal))
	(group-id 0))
    (dotimes (i (length map))
      (let* ((xy (aoc-i-xy i w h))
	     (s (member i seen)))	      
	(unless s
	  (push i seen)
	  (push (aoc-make-group group-id (aoc-scan-group i map w h))
		(gethash group-id groups nil))
	  (setq group-id (1+ group-id))
	  )))
    groups))

(setq aoc-example
      '("RRRRIICCFF"
	"RRRRIICCCF"
	"VVRRRCCFFF"
	"VVRCCCJFFF"
	"VVVVCJJCFE"
	"VVIVCCJJEE"
	"VVIIICJJEE"
	"MIIIIIJJEE"
	"MIIISIJEEE"
	"MMMISSJEEE"))

(aoc-2024-12a aoc-example)



