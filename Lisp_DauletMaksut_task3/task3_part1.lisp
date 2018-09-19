;Used code from http://ftp.ics.uci.edu/pub/machine-learning-programs/Introductory-AI/programs/a-star.lisp
;And www.ccs.neu.edu/home/rjw/csg120/programs/.../a-star.lisp

;Got from internet
(let ((adjacency-info (make-hash-table :size 20))
      (path-predecessor-info (make-hash-table :size 20)) )
  (defun set-adj (x y)
    (setf (gethash x adjacency-info) y) )
  (defun get-adj (x)
    (gethash x adjacency-info) )
  (defun set-predecessor (x y)
    (setf (gethash x path-predecessor-info) y) )
  (defun get-predecessor (x)
    (gethash x path-predecessor-info) )
 )

;Slide
(set-adj 'arad      '(366 (timisoara 118) (sibiu 140) (zerind 75)))
(set-adj 'bucharest '(0   (fagaras 211) (giurgiu 90) (pitesti 101) (urziceni 85)))
(set-adj 'craiova   '(160 (dobreta 120) (pitesti 138) (rimnicuvilcea 146)))
(set-adj 'dobreta   '(242 (craiova 120) (mehadia 75)))
(set-adj 'eforie    '(161 (hirsova 86)))
(set-adj 'fagaras   '(176 (bucharest 211) (sibiu 99)))
(set-adj 'giurgiu   '(77  (bucharest 90)))
(set-adj 'hirsova   '(151 (efofie 86) (urziceni 98)))
(set-adj 'iasi      '(226 (neamt 87) (vaslui 92)))
(set-adj 'lugoj     '(244 (mehadia 70) (timisoara 111)))
(set-adj 'mehadia   '(241 (dobreta 75) (lugoj 70)))
(set-adj 'neamt     '(234 (iasi 87)))
(set-adj 'oradea    '(380 (sibiu 151) (zerind 71)))
(set-adj 'pitesti   '(100 (bucharest 101) (craiova 138) (rimnicuvilcea 97)))
(set-adj 'rimnicuvilcea '(193 (craiova 146) (pitesti 97) (sibiu 80)))
(set-adj 'sibiu     '(253 (oradea 151) (arad 140) (rimnicuvilcea 80) (fagaras 99)))
(set-adj 'timisoara '(329 (arad 118) (lugoj 111)))
(set-adj 'urziceni  '(80 (bucharest 85) (hirsova 98) (vaslui 142)))
(set-adj 'vaslui    '(199 (iasi 92) (urziceni 142)))
(set-adj 'zerind    '(374 (arad 75) (oradea 71)))
;Glovar vars
(setq globalVar '())
(setq preDistance 0)

(defun begin (start destination)
  ;Two cases: if destination or starting point is bucharest
	(format t "Path - ")
  (cond ((eq destination 'bucharest)
    (toBuch start destination 0)
    (return-from begin '())
    )
  )
  (fromBuch start destination 0)
)

;from Bucharest
(defun fromBuch (start destination preDistance)
	(write start)
  (cond ((eq start destination)(fringeDo)))
	(loop for x in (rest (get-adj start)) do
		(setq tmp '())
		(setq est (abs (- (first (get-adj start)) (first (get-adj (first x))))))
		(setq dis (+ preDistance (second x) est))
		(setq tmp (copy-list (append tmp (list (first x)))))
		(setq tmp (copy-list (append tmp (list dis))))
		(setq globalVar (copy-list (append globalVar (list tmp))))
		(set-predecessor (first x) (+ (second x) preDistance))
	)
	(write globalVar)
	(dist2 destination)
)

(defun dist2 ( destincation)
  (setq retVal '(NotImportant 10000))
	(loop for x in globalVar do
		(if (< (second x) (second retVal)) (setq retVal x))
	)
	(rmv retVal globalVar)
	(setq preDistance (get-predecessor (first retVal)))
	(fromBuch (first retVal) destincation preDistance)
)
;End of Bucharest
;toward to Bucharest
(defun toBuch (start destination preDistance)
	(write start)
	(format t " (")
	(write preDistance)
	(format t "+")
	(write (first (get-adj start)))
	(format t ")")
	(cond ((eq start destination)(fringeDo)))
  (format t ", ")
	(loop for x in (rest (get-adj start)) do
		(setq tmp '())
		(setq dis (+ preDistance (second x) (first (get-adj (first x)))))
		(setq tmp (copy-list (append tmp (list (first x)))))
		(setq tmp (copy-list (append tmp (list dis))))
		(setq globalVar (copy-list (append globalVar (list tmp))))
		(set-predecessor (first x) (+ (second x) preDistance))
	)
	(dist1 destination)
)

(defun dist1 ( dest)
	(setq res '(NotImportant 10000))
	(loop for x in globalVar do
	   (cond ((< (second x) (second res)) (setq res x)))
	)
	(rmv res globalVar)
	(setq preDistance (get-predecessor (first res)))
	(toBuch (first res) dest preDistance)
)

(defun rmv (check list)
	(setq tmp '())
	(loop for x in list do
		(if (not (eq check x))(setq tmp (append tmp (list x))))
	)
	(setq globalVar tmp)
)
;end to Bucharest

(defun fringeDo ()
  (terpri)
  (terpri)
  (format t "Fringe - ")
  (write globalVar)
  ;Quit from function to exit
  (funcall (find-symbol "QUIT" "COMMON-LISP-USER"))
)

(begin 'bucharest  'arad)
