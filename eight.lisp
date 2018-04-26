(in-package :cl-user)

(defstruct (board (:print-function board-print-procedure))
  "Data structure representing a board"
  (arr (make-array '(3 3)))) ;array with numbers

(defun set-goal-board (arr)
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (aref arr i j) (+ (* 3 i) j 1))))
  (setf (aref arr 2 2) 0)
  arr)
 

(defun board-print-procedure (pr str ignore)
  (declare (ignore ignore))
  (dotimes (i 3)
    (format str "~%")
    (dotimes (j 3)
      (format str "| ~A |" (aref (board-arr pr) i j)))))

(defun copy-state (state)
  "Makes copy of the state"
  (let ((a (make-array '(3 3))))
       (dotimes (i 3)
	 (dotimes (j 3)
	   (setf (aref a i j) (aref (board-arr state) i j))))
       (make-board :arr a)))
    
 
(defun eight-distance (path)
  "Heuristic function for choosing the best move"
  (flet ((distance (num i j)
	   (+ (abs (- i (/ (- num 1) 3)))
	      (abs (- j (mod (- num 1) 3))))))
    (flet ((get-numb (path i j)
	     (aref (board-arr (car path)) i j)))
      (let ((sum 0))
	(dotimes (i 3)
	  (dotimes (j 3)
	    (let ((a (get-numb path i j)))
	      (if (= a 0)
		  (setq sum (+ sum (+ (abs (- i 2)) (abs (- j 2)))))
		  (setq sum (+ sum (distance a i j)))))))
	sum))))

(defun setup-eight-problem (goal-state)
  (make-problem :name goal-state
		:goal-recognizer #'(lambda (state)
		    (eight-states-identical? state goal-state))
		:operator-applier 'eight-operator-finder
		:operators '(empty-down empty-up empty-right empty-left)
		:states-identical? 'eight-states-identical?
		:state-printer #'(lambda (f) (format nil "~A" f))
		:solution-element-printer 'print-path-element
		:distance-remaining #'eight-distance))

(defun eight-states-identical? (state1 state2)
  (let ((is-eq t))
    (dotimes (i 3)
      (dotimes (j 3)
	(if (not (= (aref (board-arr state1) i j) (aref (board-arr state2) i j)))
	    (setq is-eq nil))))
    is-eq))

(defun print-path-element (to-state ignore)
  (declare (ignore ignore))
  (format nil "~A" to-state))

(defun eight-operator-finder (state op)
  (let ((next-state (funcall op state)))
    (if next-state
	(list (cons nil next-state)))))


(defun find-empty (state)
  (let ((arr (board-arr state)))
    (dotimes (i (array-total-size (board-arr state)) nil)
      (if (= 0 (row-major-aref arr i))
	  (multiple-value-bind (row column) (truncate i 3)
	    (return (cons row column)))))))

(defun move (state from to)
  (let ((new (copy-state state)))
    (rotatef (aref (board-arr new) (car from) (cdr from))
	     (aref (board-arr new) (car to) (cdr to)))
    new))

(defun move-empty (state x y)
  (let ((empty (find-empty state)))
    (flet ((out-of-bounds? (par f) (or (> (funcall f par) 2) (< (funcall f par) 0))))
      (if (or (out-of-bounds? (car empty) x) (out-of-bounds? (cdr empty) y))
	  nil
	  (move state empty (cons (funcall x (car empty)) (funcall y (cdr empty))))))))



;; OPERATORS

(defun empty-up (state)
  (move-empty state #'(lambda (x) x) #'(lambda (y) (- y 1))))

(defun empty-down (state)
  (move-empty state #'(lambda (x) x) #'(lambda (y) (+ y 1))))

(defun empty-right (state)
  (move-empty state #'(lambda (x) (+ x 1)) #'(lambda (y) y)))

(defun empty-left (state)
  (move-empty state #'(lambda (x) (- x 1)) #'(lambda (y) y)))




(defparameter START
  (make-board :arr (make-array '(3 3) :initial-contents '((4 6 3) (5 1 2) (0 8 7)))))

(defparameter GOAL
  (make-board :arr (set-goal-board (make-array '(3 3)))))


;; TESTS 

;; (defparameter START
;;   (make-board :arr (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 0 8)))))

;; (defparameter START
;;   (make-board :arr (make-array '(3 3) :initial-contents '((1 2 3) (4 5 0) (7 8 6)))))

;; (defparameter START
;;   (make-board :arr (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 0)))))

;; (defparameter START
;;   (make-board :arr (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (0 7 8)))))

;; (defparameter START
;;   (make-board :arr (make-array '(3 3) :initial-contents '((3 4 7) (2 0 6) (1 8 5)))))



(time (print-answer (best-solve START (setup-eight-problem GOAL))))






	
  
  
		    
