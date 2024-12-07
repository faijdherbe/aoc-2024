;; -*- aoc-date: (2024 6); -*-

(require 'advent-of-code)

(defun aoc-parse-map (input)
  (let ((obstacles '())
        (player nil)
        (x 0)
        (y 0))
    (dolist (line input)
      (dotimes (x (length line))
        (let ((char (elt line x)))
          (cond ((= ?# char)
                 (add-to-list 'obstacles (cons x y)))
                ((member char '(?< ?> ?^ ?v))
                 (setq player `((pos . ,(cons x y))
                                (dir . ,(aoc-dir-to-transform char))))))))
      (setq y (1+ y)))
    `((map . ,input)
      (obstacles . ,obstacles)
      (player . ,player))))                  

(defun aoc-dir-to-transform (dir)
  (cond ((= dir ?>) (cons 1 0))
        ((= dir ?<) (cons -1 0))
        ((= dir ?^) (cons 0 -1))
        ((= dir ?v) (cons 0 1))))

(defun aoc-char-at-map (loc map)
  (elt (nth (cdr loc) map) (car loc)))

(defun aoc-turn (dir)
  (cond ((equal dir (cons 1 0)) (cons 0 1))
        ((equal dir (cons 0 1)) (cons -1 0))
        ((equal dir (cons -1 0)) (cons 0 -1))
        ((equal dir (cons 0 -1)) (cons 1 0))))

(defun aoc-out-of-boundsp (loc map)
  (or (= -1 (car loc))
      (= -1 (cdr loc))
      (= (car loc) (length (nth 0 map)))
      (= (cdr loc) (length map))))

(defun aoc-translate (a dir)
  (cons (+ (car a) (car dir))
        (+ (cdr a) (cdr dir))))

(defun aoc-step (player map obstacles)
  (let-alist player 
    (let* ((new-pos (aoc-translate .pos .dir))
           (out-of-bounds (aoc-out-of-boundsp new-pos map)))
      (cond (out-of-bounds nil)
            ((member new-pos obstacles)
             `((pos . ,.pos)
               (dir . ,(aoc-turn .dir))))
            (t `((pos . ,new-pos)
                 (dir . ,.dir)))))))
             

(defun aoc-seen-pos (player)
    (let-alist player
    (goto-char (+ (car .pos) (* (1+ aoc-width) (cdr .pos))))
    (delete-char -1)
    (insert-char ?*)
    ))

(defun aoc-loopable-p (player seen obstacles)
  (let-alist player
    (and (member `((pos . ,(aoc-translate .pos (aoc-turn .dir)))
              (dir . ,(aoc-turn .dir)))
                 seen))))
       


(let-alist (aoc-parse-map (aoc-read-input))
  (let ((player .player)
        (seen '())
        (ok t))
    (add-to-list 'seen .player)
    (while ok
      (let ((result (aoc-step player .map .obstacles)))
        (if (eq nil result)
            (setq ok nil)
          (progn
            (setq player result)
            (add-to-list 'seen player)))))
    (aoc-answer (length (seq-uniq (mapcar (lambda (p)
                                            (assoc 'pos p))
                                          seen))))))

