(setq input '((A 0) ((B 5) ((C 3) ((D 4))) ((E 2) ((F 1)) ((G 7) ((H 9))))) ((J 1) ((K 3) ((L 1) ((M 7)) ((N 1) ((P 2)))))))    )



;Task 2 A
(print "* Daulet Maksut *")
(terpri)
(print "2 (Model 1):")
(terpri)
(setq input (list input))

(defun doTask2A (data)
	(loop while data
		do
		(setq elemNum (length data))
		(loop while (> elemNum 0)
			do
			(setq element (first data))
			(setq data (rest data))
			(setq printer (first element))
			(format t "~s,~d " (first printer) (first (rest printer)))
			(setq data (append data (addToChildren (rest element) (first(rest printer)) )))
			(decf elemNum)
		)
		(terpri)
	)
)

(defun addToChildren (data adder)
	(setf retList '())
	;(print data)
	(dolist (x data)
		(setq updateValue (first x))
		(setq children (rest x))
		(setq name (first updateValue))
		(setq value (first (rest updateValue)))
		(setq value (+ value adder))
		(setq updateValue (list name value))
		(setq updateValue (list updateValue))
		(setq result (list (append updateValue children)))
		;(print result)
		(setq retList (append retList result))
	)

	;(print retList)
	(return-from addToChildren retList)
)
(doTask2A input)



;Task 2 B
(print "2 (Model 2):")
(terpri)

(defun doTask2B (data)
	(loop while data
		do
		(setq elemNum (length data))
		(loop while (> elemNum 0)
			do
			(setq element (first data))
			(setq data (rest data))
			(setq printer (first element))
			(format t "~s,~d; " (first printer) (first (rest printer)))
			(setq data (append data (printChilds (rest element) (first(rest printer)) )))
			(decf elemNum)
			(terpri)
		)
	)
)

(defun printChilds (data adder)
	(setf retList '())
	;(print data)
	(dolist (x data)
		(setq updateValue (first x))
		(setq children (rest x))
		(setq name (first updateValue))
		(setq value (first (rest updateValue)))
		(setq value (+ value adder))
		(format t "~s,~d " name value)
		(setq updateValue (list name value))
		(setq updateValue (list updateValue))
		(setq result (list (append updateValue children)))
		;(print result)
		(setq retList (append retList result))
	)

	;(print retList)
	(return-from printChilds retList)
)

(doTask2B input)