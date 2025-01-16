;; -*- aoc-date: (2024 25); -*-

(require 'advent-of-code)

(defun ~parse-part (data)
  (seq-reduce (lambda (carry value)
                (logior (lsh carry 1)
                        (if (= ?# value) 1 0)))
              (substring (string-join data) 4 -5) 0))

(let ((data (aoc-read-input))
      (objects '())
      (pairs 0))
  (while (car data)
    (setq objects (push (~parse-part (take 7 data)) objects))
    (setq data (nthcdr 7 data)))
  (while (cdr objects)
    (let ((a (car objects)))
      (dolist (b (cdr objects))
        (when (= (+ a b) (logxor a b))
          (setq pairs (1+ pairs)))))
    (setq objects (cdr objects)))
  pairs)
