;;;; scripts from task 2a! Scroll down for oblig 2b


;;;; 1a) Context of a focus word will in our task be to look
;;;; at the other words sharing the same sentence. Another way
;;;; of defining context would be to look at the n-closest words
;;;; to both the left and right of our chosen focus word. Both techniques are called "Bag of Words"
;;;; More complicated techniques could be able to define grammatical contexts between words.

;;;; 2a)

(defstruct vs
  (matrix nil)
  (similarity-fn nil)
  (similarity-matrix nil)
  (classes)
  (rocchio))

;;;; 2b)

;;;;First let's define the stop list as a hash table. I've made everything into hashtables btw.
;;;;This way, we don't have to iterate through entire lists when making boolean checks to see if
;;;;co-occurence counts should be made.

(defparameter *stop-list* ;;setting *stop-list* equal to a let expression
  
   (let*((stoplist (make-hash-table :test #'equalp)))    ;; initialize hash table
    (dolist (i '("a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"  ;; for word in stop-list
		 "but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
		 "he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
		 "may" "most" "new" "no" "not" "of" "on" "or" "she" "some" "such"
		 "than" "that" "the" "their" "them" "there" "these" "they" "this"
		 "those" "to" "was" "we" "were" "what" "when" "where" "which"
		 "who" "will" "with" "would" "you"))                               
      (setf (gethash i stoplist) T))stoplist))                             ;;;; set stop-words as keys and values as true
                                                                           ;;;; this way, we can make simple bool checks
                                                                           ;;;; when filtering through the corpus without list iteration



;;;; It's going to be useful to know if a word from the corpus is in words.txt.
;;;; To quickly check if the sentence we're iterating through contins focus words -
;;;; we simply put the 122 focus words in a hashtable. Just like the opposite of a stop list.


;;;; Here we put the focus words into a parameter
(defparameter *words*
  (with-open-file                                     
      (stream "words.txt" :direction :input)
    (loop
       :for line = (read-line stream nil)
       :while line
       :collect (remove-if-not #'alphanumericp line))))

;;;; Here we create a hashtable where each of the focus words correspond to a true value.
(defparameter *word-hash*
  (let((table (make-hash-table :test #'equalp)))
    (dolist (i *words*)                          ;;i is a focus word in words.txt
      (setf(gethash i table)t))table))           ;;i becomes key, t becomes value, t = True


;;;; The matrix will be a hashtable containing 122 hashtables. The 122 hashtables will be our vectors.
;;;; To yield a vector, a key corresponding to a word in words.txt must be given.
;;;; example1: matrix(word1) -> feature_vector1 ;;;;word1 must be a word in words.txt
;;;; example2: feature_vector1(word2) -> number of co-occurences between word1 and word2.


(defparameter hash_matrix
  (let*((matrix (make-hash-table :test #'equalp)))    ;; our matrix
    (dolist (i *words*)                               ;; for word in words
      (setf (gethash i matrix) (make-hash-table :test #'equalp)))matrix)) ;;hash-key becomes word
      ;;give each hash-key (word) a corresponding hash table and returns hash table holding 122 hash tables


;;;;  We're still on 2b):
;;;;
;;;;  Now that we have defined our structures (matrix hashtable and 122 feature vector hashtables and stop-list)
;;;;  - we can move on to define our "read corpus to vs" function. To trim the corpus file, I've modified our previous
;;;;  Tokenize function to also filter out stop list words and empty tokens. Its name here is "normalize_token"
;;;;
;;;;

(defun normalize_token (string *stop-list*)
  (loop
     :for start = 0 then (+ space 1)               
     :for space = (position #\space string :start start)  
     :for token = (subseq string start space)      
     :unless (string= token "") :do (setf token (remove-if-not #'alphanumericp (string-downcase token)))
     :if (> (length token) 0) :if (equal (gethash token *stop-list*) nil) :collect token
     :until (not space)))  ;;removes empty strings, checks if word in stop list
      


;;;; The co occurence counting is implented here

(defun read-corpus-to-vs (corpus_filename)
  (with-open-file      
      (stream corpus_filename :direction :input)
    (loop
       :for line = (read-line stream nil)
       :while line
       :do (let (( sentence (normalize_token line *stop-list*)))  ;The sentence in corpus is tokenized and normalized
	     (dolist (word1 sentence)                             ;For word1 in sentence
	       (cond ((gethash word1 *word-hash*)                 ;if word1 is one of our 122 focus words
		      (let ((sentence1 (remove word1 sentence)))  ;remove the focus word, word1, from the sentence
			(dolist (word2 sentence1)                 ;now we count every remaining word2 into the co-occurence vector of the focus word1
			  (if (gethash word2 (gethash word1 hash_matrix))
			      (incf (gethash word2 (gethash word1 hash_matrix)))   ;;increments co-occurence value by 1
			      (setf (gethash word2 (gethash word1 hash_matrix))1)  ;;sets co-occurence to 1 if first co-occurence between word1 and 2
			      )))))))))hash_matrix)  ;;returns vector space structure with co-occurence matrix inside.



(defparameter space-vs (make-vs
		:matrix (read-corpus-to-vs "brown2.txt"))) ;;;;as you can tell, my function does not call "words.txt"
                                                         ;;;;and I think you will agree that this makes the code more readable.


;(print (gethash "university" (gethash "university" vs-matrix)))

;(maphash #'(lambda (k v) (print (list k v)))   (gethash "university"  hash_matrix))


;;;; 2c

;;;; Our struct contains a hash table which contains 122 hashtables which we call our feature vectors
;;;; We can therefore use a gethash expression to yield the feature vector of a word:

(defun get-feature-vector (space key)
  (gethash key (vs-matrix space)))
;(get-feature-vector space-vs "potato")

;;;; 2d

;;;;The idea in this bit is to create an object corresponding to the feature vector with let()
;;;;I then gather all the hash-values (co-occurences) in a list called List_count with the maphash function
;;;;Then, I sort the largest co-occurence values and use them as loop variables to find keys that correspond


(defun print-features (space key k)
  (let((fix (list )))
    (let((vector (get-feature-vector space key)))  ;;vector
      (let((List_count (list )))                   ;;co-occurences
	(maphash #'(lambda (K v)
		     (setf List_count (append List_count (list v))))vector)
	
	(setf List_count (sort List_count #'>))    ;;sorting co-occurence so only largest values are iterated
	(loop for i from 0 to (- k 1)              ;;loop for i from 0 to 8 which is used to -
	   do (let (( Max (nth i List_count)))     ;;loop through 9 largest values of co-occurence (calling loop variable for Max)
		(loop for key2 being the hash-keys of vector  ;;loop through entire vector to find key corresponding to largest co-occurence value (Max) 
		   :if (eq (gethash key2 vector) Max)         ;;if the key corresponds to the max value
		   :do(format t "~a ~a ~%" key2 Max)          ;;print key and max value
		   :and :do (setf (gethash key2 vector) 0)    ;;remove Max value so that repetitions don't take place
		   :and :do  (setf fix (append fix (list key2))) ;;those removals happened globally, which is bad
		   :and :do (return))))                          ;;I made a fix list to remember which keys and values got removed.
	(loop for i from 0 to (- k 1)         ;;here I reinstate the values that was set to zero so that our similarity dot product isn't affected
       	  :do (setf (gethash (nth i fix) vector) (nth i List_count))) 
		
			 ))))    


;(print-features space-vs "university" 9)  ;;;; heres a function call for the word university

;;;; 3a Euclidean norm

(defun euclidean-norm (vector) ;;takes vector, returns sqrt of sum of squred elements
  (let (( sum (apply '+ (loop :for v being the hash-values of vector ;;for value in vector
			 :collect (* v v)))))(sqrt sum)))  ;;returns square root of sum of products

;(euclidean-norm (get-feature-vector space-vs "boston"))

;;;; 3b

(defun length-normalize-vs (space)   ;vector space struct as argument
  (let ((matrix (vs-matrix space)))  ;yield matrix
    (loop for vector being the hash-values of matrix     ;;for vector in matrix
       :do (let((norm (euclidean-norm vector) ))         ;;define norm
	     (loop for key being the hash-keys of vector ;;for value in vectot
		:do (setf (gethash key vector) (/  (gethash key vector) norm))))) ;;divide every value with norm
    (setf space (make-vs :matrix matrix))))              ;;return normalized matrix into vector space, space-vs

(length-normalize-vs space-vs)

;;;; 3c

(defun dot-product (v1 v2)
  (apply '+ (loop for key being the hash-keys of v1  ;;using apply to sum the products
	       :if (gethash key v2) :collect (* (gethash key v2) (gethash key v1)))))

(setf (vs-similarity-fn space-vs) #'dot-product)     ;;puttng the dot product into our struct

;;;; 3d
(defun word-similarity (space-vs word1 word2)
  (let ((v1 (gethash word1 (vs-matrix space-vs))))   ;;collecting the vectors from our matrix
    (let ((v2 (gethash word2 (vs-matrix space-vs)))) ;;in order to call the dot-product in our struct
      (funcall(vs-similarity-fn space-vs)  v1 v2)))) ;;- with the help of (funcall)

;(word-similarity space-vs "university" "university")  ;;just a test




;;;;OBLIG 2B

;;;; 1a
(defun compute-proximities (space-vs)
  (let ((sim-mat (make-hash-table :test #'equal)))
    (dolist (word1 *words*)
      (setf (gethash word1 sim-mat)
	    (let ((vec (make-hash-table :test #'equal)))
	      (dolist(word2 *words*)
		(setf (gethash word2 vec)
		      (word-similarity space-vs word1 word2)))vec)))
    (setf (vs-similarity-matrix space-vs) sim-mat)  ))

;;returns similarity vector into similarity matrix
(compute-proximities space-vs)



(defun get-proximities (space word1 word2)
  (let ((mat (vs-similarity-matrix space)))
    (let((vec (gethash word1 mat)))
      (gethash word2 vec))))

;(print(get-proximities space-vs "potato" "food"))


;;;;1b

(defun knn (space word k)
  (let (( words (list )))
    (let (( unsorted (list )))
      (let (( sorted (list )))
	(setf words (loop
		       :for Word being the hash-keys in (gethash word (vs-similarity-matrix space))
		       :collect Word))
	(setf unsorted (loop
			:for proximity being the hash-values in  (gethash word (vs-similarity-matrix space))
			:collect proximity))
	(setf sorted (sort unsorted #'>))	
	(loop
	   :for i from 1 to k
	   :collect (nth (position (nth i sorted) unsorted  ) words)
	   )))))

;(print(knn space-vs "potato" 20))
;;;;2a
(defun normalize_word (string)
  (setf string (remove-if-not #'alphanumericp (string-downcase string))))


(defun read-classes (filename)
  (with-open-file (in filename :direction :input)
    (let(( classes (make-hash-table :test #'equal)))
      (loop
	 :for i = (read in nil nil)  
	 while i
	 :do (setf(gethash  (normalize_word (car i) )  classes)
		  (mapcar #'normalize_word (car(cdr i) ))))
      (setf (vs-classes space-vs) classes))))

(read-classes "classes.txt")

;;;; 2b 2b

;;;; this task requires me to create a "vector add" function
;;;; and a normalize-centroids function (based on normalizing a matrix in oblig2a)

(defun normalize-centroids (centroids)   ;vector space struct as argument
    (loop for centroid being the hash-values of centroids  ;;for centroid
       :do (let((norm (euclidean-norm centroid) ))         ;;define norm
	     (loop for key being the hash-keys of centroid ;;for value in vectot
		:do (setf (gethash key centroid) (/  (gethash key centroid) norm))))) ;;divide every value with norm
    centroids)             ;;return normalized matrix into vector space, space-vs


(defun vector-add (v1 v2) ;;Having this guy makes the centroids easier to compute
  (let (( v3 (make-hash-table :test #'equal)))
    (loop for keys being the hash-keys in v2	 
       :do (if (gethash keys v1) (setf (gethash keys v3) (+ (gethash keys v1) (gethash keys v2))))
       :do (unless (gethash keys v1) (setf (gethash keys v3) (gethash keys v2))))
    (loop for keys being the hash-keys in v1	 
       :do (if (gethash keys v2) (setf (gethash keys v3) (+ (gethash keys v1) (gethash keys v2))))
       :do (unless (gethash keys v2) (setf (gethash keys v3) (gethash keys v1))))v3))

(defun compute-class-centroids (space)
  (let ((centroids (make-hash-table :test #'equal)))
    (let ((class-hash (vs-classes space)))
      (let ((feature-vectors (vs-matrix space)))
	(loop
	   :for class being the hash-keys in class-hash
	   :do (setf (gethash class centroids) (let (( feat-vec (make-hash-table :test #'equal)))
						 (loop
						    :for word in (gethash class class-hash)
						    :for vec = (vector-add feat-vec (gethash word feature-vectors))
						    :then (vector-add vec (gethash word feature-vectors))
						    :do (setf feat-vec vec))(/ feat-vec (length  (gethash class class-hash) )) )))))
    
    (let (( centroids (normalize-centroids centroids)))
      (setf (vs-rocchio space-vs) centroids))))

;;;;2c
(defun rocchio-classify (space)
  (let (( centroids  (vs-rocchio space)))
