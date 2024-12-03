;; -*- aoc-date: (2024 3); -*-

(require 'advent-of-code)

;; xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))

(setq aoc-mul-regexp "mul(\\([0-9]+\\),\\([0-9]+\\))")

(defun aoc-parse-mul (line &optional start)
  (when (string-match-p aoc-mul-regexp line start)
    (let* ((offset (string-match aoc-mul-regexp line start))
	   (match (match-string 0 line))
	   (a (string-to-number (match-string 1 line)))
	   (b (string-to-number (match-string 2 line)))
	   (rest (aoc-parse-mul (substring line (+ offset (length match))))))
      (+ (* a b) (or rest 0)))))


(aoc-answer (apply '+ (mapcar 'aoc-parse-mul
			      (aoc-read-input))))
