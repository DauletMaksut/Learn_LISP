(setf input '(A (B (E) (F) (G)) (C (H) (I) (J)) (D (K) (L) (M)))   )
(setf node "I")


(print "Made by: DAULET MAKSUT")
(terpri)
(print "Task 2, part 1:")


(defun bfs (input)
	(format t "~S " (first input))
	(setq depth 0)
 	(loop do
    	(if (findRecursion input (setf depth (incf depth)))
        	(return-from bfs t)
        )
 	)
)

(defun findRecursion (text currentLevel)
	(setq childrens (rest text))
	(if (string= node (first text)) 
		(return-from findRecursion t)
	)
	(if (= currentLevel 0)
		(return-from findRecursion NIL)
	)

	(dolist (childs childrens)
		(format t "~S " (first childs))
		(setq nextLevel (- currentLevel 1))

		(if (findRecursion childs nextLevel)
			(return-from findRecursion t)
		)
		(format t "~S " (first text))
	)
	(return-from findRecursion NIL)
)
(terpri)
(bfs input)



(terpri)
(print "Task 2, part 2:")
(terpri)
(defun printA (text)
	(format t "~S " (first text))
	(easePeasy text)
)

(defun easePeasy (text)
	(setq childrens (rest text))
	(if (string= node (first text)) 
		(return-from easePeasy t)
	)

	(dolist (childs childrens)
		(format t "~S " (first childs))

		(if (easePeasy childs)
			(return-from easePeasy t)
		)
		(format t "~S " (first text))
	)
	(return-from easePeasy NIL)
)

(printA input)

