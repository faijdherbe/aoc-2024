;; -*- aoc-date: (2024 23); -*-

(require 'advent-of-code)

(setq aoc-input '(
"kh-tc"
"qp-kh"
"de-cg"
"ka-co"
"yn-aq"
"qp-ub"
"cg-tb"
"vc-aq"
"tb-ka"
"wh-tc"
"yn-cg"
"kh-ub"
"ta-co"
"de-co"
"tc-td"
"tb-wq"
"wh-td"
"ta-ka"
"td-qp"
"aq-cg"
"wq-ub"
"ub-vc"
"de-ta"
"wq-aq"
"wq-vc"
"wh-yn"
"ka-de"
"kh-ta"
"co-tc"
"wh-qp"
"tb-vc"
"td-yn"
))

(defun aoc-register-pair (pair table)
  (puthash (car pair)
	   (push (cdr pair)
	         (gethash (car pair)
		          table
		          nil))
	   table)
  (puthash (cdr pair)
	   (push (car pair)
	         (gethash (cdr pair)
		          table
		          nil))
	   table))


(defun aoc-chief-group-p (g)
  (length> (seq-filter (lambda (c)
                         (= ?t (elt c 0)))
                       g)
           0))

(defun aoc-map-networks (table)
  (let (out (keys (hash-table-keys table)))
    (while keys
      (let* ((key (car keys))
             (nkey (cadr keys))
             (int (seq-intersection (cons key (gethash key table))
                                    (cons nkey (gethash nkey table)))))
        (setq keys (cdr keys))
        (when (= 3 (length int))
          (add-to-list 'out int 'string<))))
      out))

(let* ((table (make-hash-table :test 'equal))
       (input aoc-input)
       (input (mapcar (lambda (l)
			(let ((ll (string-split l "-")))
			  (cons (car ll)
				(cadr ll))))
		      input))
       (input (mapcar (lambda (x)
			(aoc-register-pair x table)
			x)
		      input)))
  (let* ((networks (aoc-map-networks table))
         (groups (seq-filter (lambda (g)
                                      (not (member nil g)))
                                    networks))
         (groups (seq-filter 'aoc-chief-group-p
                             groups)))
    (debug groups)
    (aoc-answer (length groups))))
