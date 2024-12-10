;; -*- aoc-date: (2024 9); -*-

(require 'advent-of-code)

(setq aoc-example "2333133121414131402")

(defun aoc-file-block-p (index)
  (= 0 (% index 2)))

(defun aoc-id-from-index (idx)
  (ceiling (/ idx 2)))


(defun aoc-get-files (disk amount)
  (let ((out '())
	(disk disk))
    (dotimes (i amount)
      (unless (= 0 (length disk))
      (while (= ?0 (elt disk (1- (length disk))))
	(setq disk (substring disk 0 (* -1 (min 2 (length disk))))))
      (let* ((idx (1- (length disk)))
	     (c (- (elt disk idx) ?0)))
	(setq out (append out (list (aoc-id-from-index idx))))
	(setq disk (concat (substring disk 0 -1)
			   (list (+ (1- c) ?0)))))))
    (cons out disk)))

(defun aoc-generate-files (idx amount disk)
  (cons (make-list amount (aoc-id-from-index idx)) disk))
	
(defun aoc-2024-09-a (disk)
  (let* ((index 0)
	 (checksum 0)
	 (disk disk)
	 (offset 0))
    (while (< index (length disk))
      (let* ((amount (- (elt disk index) ?0))
	     (is-file (aoc-file-block-p index))
	     (files (if is-file
			(aoc-generate-files index amount disk)
		      (aoc-get-files disk amount))))
	(setq checksum
	      (seq-reduce (lambda (carry v)
			   (let ((r (+ carry (* offset v))))
			     (setq offset (1+ offset))
			     r))
		      (car files)
		      checksum))
	(setq disk (cdr files))
	(setq index (1+ index))))
    checksum))

(aoc-answer (aoc-2024-09-a (car (aoc-read-input))))

