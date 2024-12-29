;; -*- aoc-date: (2024 16); -*-

(require 'advent-of-code)


(setq aoc-2024-16 '("###############"
                    "#.......#....E#"
                    "#.#.###.#.###.#"
                    "#.....#.#...#.#"
                    "#.###.#####.#.#"
                    "#.#.#.......#.#"
                    "#.#.#####.###.#"
                    "#...........#.#"
                    "###.#.#####.#.#"
                    "#...#.....#.#.#"
                    "#.#.#.###.#.#.#"
                    "#.....#...#.#.#"
                    "#.###.#.#.#.#.#"
                    "#S..#.....#...#"
                    "###############"))


(defun dijkstra--neighbours (u vertices)
  (let ((n '())
        (tr '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))))
    (dolist (tt tr)
      (let ((vv (cons (- (car u) (car tt))
                      (- (cdr u) (cdr tt)))))
        (when (member vv vertices)
          (add-to-list 'n vv))))
    n))

(defun dijkstra--dir (a b)
  (let* ((b (or b (cons (1- (car a)) (cdr a)))) ;; default facing: east
         (dir (cons (- (car a) (car b))
                   (- (cdr a) (cdr b)))))
    (cond ((equal dir (cons 0 -1))
           'north)
          ((equal dir (cons 1 0))
           'east)
          ((equal dir (cons 0 1))
           'south)
          ((equal dir (cons -1 0))
           'west))))

(defun dijkstra--cost (d s ss)
  (if (eq (dijkstra--dir d s)
          (dijkstra--dir s ss))
        1
    1001))

(defun dijkstra (vertices start)
  (let ((dist (make-hash-table :test 'equal))
        (prev (make-hash-table :test 'equal))
        (Q vertices))
    (puthash start 0 dist)
    (while Q
      (let* ((QQ (seq-sort (lambda (a b) (< (gethash a dist most-positive-fixnum)
                                            (gethash b dist most-positive-fixnum)))
                           Q))
             (u (car QQ)))
        (setq Q (remove u Q))
        (dolist (v (dijkstra--neighbours u vertices))
          (when (member v Q)
            (let* ((g (gethash u dist most-positive-fixnum))
                   (h (dijkstra--cost v u (gethash u prev nil)))
                   (alt (+ g h)))
              (when (< alt (gethash v dist most-positive-fixnum))
                (puthash v alt dist)
                (puthash v u prev)))))))
      (list (cons 'dist dist)
            (cons 'prev prev))))

(defun aoc-parse-map (input)
  (let ((vertices '())
        start exit)
    (dotimes (y (length input))
      (let ((row (nth y input)))
        (dotimes (x (length row))
          (let ((c (elt row x))
                (pos (cons x y)))
            (cond ((= c ?.)
                   (add-to-list 'vertices pos))
                  ((= c ?S)
                   (add-to-list 'vertices pos)
                   (setq start pos))
                  ((= c ?E)
                   (add-to-list 'vertices pos)
                   (setq exit pos)))))))
    (list (cons 'vertices vertices)
          (cons 'start start)
          (cons 'exit exit))))

;; PART A
(aoc-answer
 (let-alist (aoc-parse-map (aoc-read-input))
   (let ((exit .exit))
     (let-alist (dijkstra .vertices .start)
       (gethash exit .dist)))))

