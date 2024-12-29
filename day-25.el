;; -*- aoc-date: (2024 25); -*-

(require 'advent-of-code)

(defun ~parse-part (input)
  (let ((data 0)
	(partdata (string-join (take 5 (cdr input)))))
    (dotimes (i (length partdata))
      (when (= ?# (elt partdata i))
	(setq data (logior data (ash 1 i)))))
    (if (= ?# (elt (car input) 0))
	(~make-lock data)
      (~make-key data))))

(defun ~make-lock (data)
  (cons 'lock data))

(defun ~make-key (data)
  (cons 'key data))

(defun ~keyp (key)
  (and (consp key)
       (eq 'key (car key))))

(defun ~parse-input (input)
  (let ((input input)
	keys locks)
    (while input
      (let ((part (~parse-part (take 7 input))))
	(if (~keyp part)
	    (setq keys (push part keys))
	  (setq locks (push part locks))))
      (setq input (nthcdr 7 input)))
    (list 'locks locks 'keys keys)))


(setq ~data '(
"#####"
".####"
".####"
".####"
".#.#."
".#..."
"....."
"#####"
"##.##"
".#.##"
"...##"
"...#."
"...#."
"....."
"....."
"#...."
"#...."
"#...#"
"#.#.#"
"#.###"
"#####"
"....."
"....."
"#.#.."
"###.."
"###.#"
"###.#"
"#####"
"....."
"....."
"....."
"#...."
"#.#.."
"#.#.#"
"#####"
))

(let* ((data (~parse-input (aoc-read-input)))
       (locks (plist-get data 'locks))
       (keys (plist-get data 'keys))
       (pairs '()))
  (dolist (lock locks)
    (let ((matches (seq-filter (lambda (key)
				 (= 0 (logand (cdr lock)
					      (cdr key))))
			       keys)))
      (setq pairs (append pairs (mapcar (lambda (key)
					  (cons (cdr key) (cdr lock)))
					matches)))))
  (aoc-answer (length pairs)))

