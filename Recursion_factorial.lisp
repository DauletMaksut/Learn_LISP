(print "Let me see")
; O'Relly example which worked bad
(defun infiniteRecursion ( n &optional(intermediate 1))
	(if (= n 1)
		(return-from infiniteRecursion intermediate)
	)
	(infiniteRecursion (1- n)(* n intermediate))
)

(print (infiniteRecursion 1000))