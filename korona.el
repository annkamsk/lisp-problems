(defun korona (lista)
  (cond ((null lista) ())
   ((atom lista) (list lista))
   (t (append (korona (car lista))
	    (korona (cdr lista))))))
(korona '(1 ((2 3) 4 (5)) () 6 (7)))
(korona '(()))

(defun f (p1 p2 &optional (o1 1)))
