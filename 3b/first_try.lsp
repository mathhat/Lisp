
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;READ


(defun read-grammar (file)
  (let((gram (make-grammar))
       (rules (make-hash-table :test #'equal))
       (memes (make-hash-table :test #'equal)))
    ;;
    ;; _fix_me_ 
    ;; this function reads in a treebank file, records the rules and lexemes seen
    ;; and, using Maximum Likelihood Estimation, calculates and stores (in the
    ;; grammar) log probabilities for each rule and lexeme, and finally returns
    ;; the grammar. 
    ;;
    (setf (gethash 'S rules) (make-hash-table :test #'equal)) ;;hardcoded
    (setf (gethash 'NP rules) (make-hash-table :test #'equal)) ;;that's bad
    (with-open-file (stream file :direction :input)
      (loop
	 :for line = (read stream nil )
	 :while line
	 :for len = (length line)
	 :for first = (first line)
	 :do (loop                    ;;find a way to...loop down branches
		:for i in (rest line)

		:do (loop for j in  (rest i)
		       :do (if (listp  j)
			       (if (stringp (nth 1 j))
				   (print j))
			       (if (eq (type-of (gethash (first i) memes)) 'hash-table)
				   (if (gethash j (gethash (first i) memes))
				       (incf (gethash j (gethash (first i) memes))))
				   (progn (setf (gethash (first i) memes) (make-hash-table :test #'equal))
					  (setf (gethash j (gethash (first i) memes)) 1)))))
					
				   
		
		:with categories = (list 0)
		:do (push (first i) (cdr(last  categories)))
		:finally 
		(if (gethash (cdr categories) (gethash first rules))
		    (incf (gethash (cdr categories) (gethash first rules)))
		    (setf (gethash (cdr categories) (gethash first rules)) 1)) )))
		
		
    (setf (grammar-rules gram) rules)
    gram))
		    
  
(defparameter gram (read-grammar "toy.mrg"))
#|(loop
for key being the hash-keys in (gethash 'NP (grammar-rules gram))
for val being the hash-values in (gethash 'NP (grammar-rules gram))
do (print key)
do (print val))
|#
(loop
   for key being the hash-keys in (gethash NNP (grammar-memes gram))
   for val being the hash-values in (gethash NNP (grammar-memes gram))
   do (print key)
   do (print val))

;;;;second try

	 :for line = (read stream nil )
	 :while line
	 :for len = (length line) ;;number of branches
	 :for first = (first line)
	 :do (loop :for branch in (rest line) :do
		(loop
		   :for vine in branch
		   :if (listp vine) :do
		   (loop :for cell = vine :then (rest cell) :do
		      (if (stringp (nth 1 cell))
			  (progn
			    (print cell)
			    (if (gethash (first cell) memes)
				(if (gethash (last cell) (gethash (first cell) memes))
				    (incf (gethash (last cell) (gethash (first cell) memes))))
				(progn
				  (setf (gethash (first cell) memes) (make-hash-table :test #'equal))
				  (setf (gethash (last cell) (gethash (first cell) memes)) 1)))
			    (setf cell nil))
			  (print cell))
		      :until (not cell))
		   
		   ))))
