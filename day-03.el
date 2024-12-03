;; -*- aoc-date: (2024 3); -*-

(require 'advent-of-code)

;;; Part 1
(defun aoc-parse-mul (line)
  (let ((aoc-mul-regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"))
    (when (string-match-p aoc-mul-regexp line)
      (let* ((offset (string-match aoc-mul-regexp line))
	     (match (match-string 0 line))
	     (a (string-to-number (match-string 1 line)))
	     (b (string-to-number (match-string 2 line)))
	     (rest (aoc-parse-mul (substring line (+ offset (length match))))))
        (+ (* a b) (or rest 0))))))


(aoc-answer (apply '+ (mapcar 'aoc-parse-mul
			      (aoc-read-input))))

  
;;; Part 2
(let ((line (string-join (aoc-read-input) ""))
      (enabled t)
      (regexp "mul(\\([0-9]+\\),\\([0-9]+\\))\\|do()\\|don't()")
      (total 0))
  (while (string-match-p regexp line)
    (let ((offset (string-match regexp line))
          (match (match-string 0 line)))
      (cond ((string= "do()" match)
             (setq enabled t))
            ((string= "don't()" match)
             (setq enabled nil))
            (enabled
             (let ((a (string-to-number (match-string 1 line)))
	           (b (string-to-number (match-string 2 line))))
               (setq total (+ total (* a b))))))
      (setq line (substring line (+ offset (length match))))))
  (aoc-answer total))
          
