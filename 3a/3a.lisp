;;;; 1a
;;;;we are given 9 tags with our sentence:
;;;;RB , NNP POS NN VBZ VBG VBN .

;;;; 1a)i) P(t|RB) is the probability of a RB being followed by a tag t,
;;;; where t is any of the 9 tags above.
;;;; there are 4 instances of RB showing up and the tags followed by each is
;;;; (,) twice
;;;; (RB) once and
;;;; (.) once.
;;;; 4 instances, where comma (,) claims two of them means that there is a
;;;; 50% chance for P(,|RB) and 25% for P(RB|RB) and P(.|RB).

;;;; 1a)ii)
;;;; There are zero instances of move being tagged as NNP, this gives us
;;;; a zero probability for P(move|NNP).

;;;; There is only one instance of a common noun (NN) -
;;;; and its observed word value is "move", meaning that P(move|NN) = 1

;;;; P(well|RB) is an instance of 1 in 4 RBs, giving us a 1/4 chance that
;;;; the word connected to the RB tag will be "well".

;;;; 1b)
;;;; The language models we're creating tries to define context with
;;;; co-occurences. Problem is, not all corpuses and context definitions
;;;; are a good representation for the relations between words in a
;;;; language. Smoothing is the appliance of "extra" co-occurence counts
;;;; where none were observed. An example is a special case of the Laplace
;;;; smoothing, called add-one smoothing. Here, you turn every 0 in your
;;;; co-occurence matrix into a 1 before normalizing.

;;;; The P(t|RB) values in 1a)i) look slightly different after applying
;;;; Laplace/add-one smoothing.
;;;; P_Smooth(t|RB) = (C(t|RB) + 1) / (C(RB) + 1*C(t))
;;;; Above, you see the laplace smoothing formula where alpha is set to 1.
;;;; C() is a counting function, meaning C(RB) = 4 occurences, and C(t)
;;;; = 9 tags. 

;;;; Before, P(,|RB) was 1/2. Now, P_smooth(,|RB) = (2+1)/(4+9) = 3/13

;;;; Before, P(RB|RB) and P(.|RB) were 1/4, now: (1+1)/13 = 2/13
;;;; These 3 tags now accumulate to P_s(,|RB) + P_s(RB|RB) + P_s(.|RB) = 7/13
;;;; It is perhaps easy to now see that our remaining 6 tags have a uniform
;;;; 1/13th of chance to occur after the RB tag, although they don't do so 
;;;; in our example sentence.

;;;; 1b)2) <NN,POS> = P(POS,NN) = P(NN|POS) * P(POS) 
;;;;                = 1 * P(POS) =  1/13           (since pos's 1 of 13 tags)
;;;; The above calculation disregards the order of the tags.
;;;; If we want the probability of NN followed by POS, then P = 0 since this is
;;;; the opposite of the order in which the tags occur. How I've undestood it,
;;;; however, is that Bigrams seem to ignore order.

;;;; 1c)
;;;; Viterbi algorithm find the optimal (most probable) sequence of states
;;;; without having to account for all permutations of the problem's states.
;;;; The way the algorithm does this for HMMs is that it leaves out the less
;;;; probable state-transition sequences that leads to the same observational
;;;; point. This is a form of dynamic programming.

;;;; A cell in a trelli diagram depicts a state. The cells before it
;;;; compose multiple sequences of states which all can lead up to this state.
;;;; These sequences each have their own probability.

;;;; If we define our sequence to have length L and let the number of state
;;;; labels be N the complexity of the algorithm is L + N + L^2*N or O(NL^2).

;;;; Task 2
;;;; 2a)

(defstruct hmm
  (states ) ;; tags
  (n)      ;; (number of tags)
  (transitions ) ;;transition probabilities
  (emissions   )   ;;emission probabilities
  )
;;;;Our states could be a hastable that accepts tags and returns their numeric value 
;;;;Our n value is simply the number of different types of tags

;;;;The transitions are simply P(i|j), where i and j are numbers representing tags,
;;;;which means they can efficiently be implemented into a matrix.

;;;;The emission values however are structured like P(word,tag=i) = P(word|tag=i)
;;;;To translate this into an efficient datastructure, we need to be able to call
;;;;a function/structure that accepts both a number and then a word as input arguments.
;;;;Sound like an array full of hashtables!

;;;; 2b)

(defun transition-probability (M i j &optional (default 0))
  (let (( p  (aref (hmm-transitions M) i j)))  ;;find the transition matrix element of state i and j
    (if (= p 0) default p)))                   ;;if it has a zero prob, we return the optional argument


(defun emission-probability (M i word &optional (default 0))
  (let (( e-array (hmm-emissions M)))          ;;emission array (list)
      (gethash word (nth (-(state2id M i) 1) e-array) default))) ;;get the hashvalue of form/word
                                                                 ;;given a tag i. i is in string form 
		     
(defun state2id (M tag)
  (let ((tags (hmm-states M)))
    (let ((index (position tag tags :test #'equal))) ;;returns nil if element doesn't exist in list
      (if index  ;if nil, push new tag into states, if already in list, return its numeric value (its index)
	  index
	  (push tag (cdr(last tags))))
      (position tag tags :test #'equal))))
	  
       

;;;; 3
;;;; 3a)

(defun read-corpus (file N-state)
  (let(( M (make-hmm :states (list 0) :n N-state)))
    (let (( L (+ N-state 1)))                 ;number of states (s and /s are = 0)
      (let(( t-mat (make-array (list L L))))  ;transition matrix
	(let(( e-array  (list )))          ;emition is a tuple where each index is a hash
	  (loop for i from 0 to (- N-state 1)
	     :do (push (make-hash-table :test #'equal) e-array)) ;create hashes which will hold emition probabilities
	  (with-open-file (stream file :direction :input)
	    (loop
	       :for i = 0 :then (+ i 1)
	       :with forms = (list 0) :with states = (list 0) ;copy paste from the evaluation.lisp file
	       :for line = (read-line stream nil)             
	       :for tab = (position #\tab line)
	       :for form = (subseq line 0 tab)
	       :for state = (and tab (subseq line (+ tab 1)))
	       :while line
	       :do (print i)
	       :unless (and form state) :do ;if there's no state or form, it's
	       (push 0 (cdr(last states)))  ;an s or /s, which both are zeros
					;in my state notation. "H", "C" and 0
	       :do (if (> i 0) ;after one iteration, we can start counting bigram state-occurences
		       (incf (aref t-mat (nth  i states) (nth ( - i 1) states)))) 
	       :when (and form state) :do
	       (push form (cdr(last forms))) ;;adding forms and states to a list for counting (above if expression)
	       (push (state2id M state) (cdr(last states)))
	       (if (gethash form (nth (- (state2id  M state)1) e-array))  ;;counting emissions
		   (incf (gethash form (nth (- (state2id  M state)1) e-array)))
		   (setf (gethash form (nth (- (state2id  M state)1) e-array)) 1))))
	  
	  (setf (hmm-emissions M) e-array)
	  (setf (hmm-transitions M) t-mat)
	  M)))))
	  
	
#|
(defun read-corpus (file N-state)
  (let(( M (make-hmm :states (list 0) :n N-state)))
    (let (( L (+ N-state 1)))                 ;number of states (s and /s are = 0)
      (let(( t-mat (make-array (list L L) )))  ;transition matrix
	(let(( e-array  (list )))          ;emition is a tuple where each index is a hash
	  (loop for i from 0 to (- N-state 1)
	     :do (push (make-hash-table :test #'equal) e-array)) ;create hashes which will hold emition probabilities
	  (with-open-file (stream file :direction :input)
	    (loop
	       :for i from 0 to 100
	       :with forms = (list 0) :with states = (list 0) ;copy paste from the evaluation.lisp file
	       :for line = (read-line stream nil)             
	       :for tab = (position #\tab line)
	       :for form = (subseq line 0 tab)
	       :for state = (and tab (subseq line (+ tab 1)))
	       :unless (and form state) :do ;if there's no state or form, it's
	       (push 0 (cdr(last states)))  ;an s or /s, which both are zeros
					;in my state notation. "H", "C" and 0
	       :do (if (> i 0) ;after one iteration, we can start counting bigram state-occurences
		       (incf (aref t-mat (nth ( - i 1) states)  (nth  i states)))) 
	       
	       :when (and form state) :do
	       (push form (cdr(last forms))) ;;adding forms and states to a list for counting (above if expression)
	       (push (state2id M state) (cdr(last states)))
	       (cond((gethash form (nth (- (state2id  M state)1) e-array))  ;;counting emissions
		     (incf (gethash form (nth (- (state2id  M state)1) e-array))))
		    (t
		     (setf (gethash form (nth (- (state2id  M state)1) e-array)) 1)))))
	  
	  
	  
	  (setf (hmm-emissions M) e-array))
	(setf (hmm-transitions M) t-mat)))
    M))

|#	

(defparameter eisner (read-corpus "eisner.tt" 2))

(defun train-hmm (M)
  (let
      ((t-mat (hmm-transitions M) )
       (e-array (hmm-emissions M) )
       (n  (hmm-n M)))
    (loop
       :for i from 0 to n
       :for sum-trans = 0 then 0
       :for sum-emit = 0 then 0  ;;things we divide by
       :do (loop
	      :for j from 0 to n
	      :do (setf sum-trans (+ sum-trans (aref t-mat i j))))
       :if (< i n) :do
       (loop
	  :for counts being the hash-values of (nth i e-array)
	  :do (setf sum-emit (+ sum-emit counts)))
       
       :if (and (> i 0) (> sum-trans 0)) :do
       (loop
	  :for j from 1 to n
	  :do (setf (aref t-mat i j) (log(/ (aref t-mat i j) sum-trans))))
       
       :if (and (< i n) (> sum-emit 0)) :do
       (loop
	  :for keys being the hash-keys of (nth i e-array)
	  :using (hash-value n_)
	  :do (setf (gethash keys (nth i e-array))
		    (log(/ n_  sum-emit)))))(print t-mat))) ;;turn probs into log values

(train-hmm eisner)

;;;;4a)

(defun Viterbi(M observations) ;;not sure if works, see output/prints
  (let*(( obs (length observations))
	( num-st (length ( cdr (hmm-states M))))
	( states (rest(hmm-states M)))
	( smooth (log 1/1000000))
	( viterbi (make-array (list ( + num-st 2) ( + obs 2))))
	( backtrace (make-array (list ( + num-st 2) (+ obs 2)))))
    (loop
       :for s in states ;;initial state probabilities
       :with o = (nth 0 observations)
       :do (setf (aref viterbi (state2id M s) 0)
		 ( + (transition-probability M 0 (state2id M s) smooth)
		     (emission-probability M s o smooth))))    
    (loop
       :for timestep from 1 to (- obs 1) ;;the rest
       :for o in (rest observations) :do
       (loop
	  :for s in states :do ;;curent
	  (loop
	     :for s_ in (rest (hmm-states M)) ;;previous
	     :for trans =  (transition-probability M (state2id M s_) (state2id M s) smooth) 
	     :for vito = (aref viterbi (state2id M s_) ( - timestep 1)) ;;earlier path probability
	     :for emit = (emission-probability M s o smooth)
	     :for p = (+ trans emit vito) 
	    
	     :do (cond( (or (> p (aref viterbi (state2id M s) timestep))
			    (= (aref viterbi (state2id M s) timestep) 0))
		       (setf (aref viterbi (state2id M s) timestep) p)
			(setf (aref backtrace (state2id M s) timestep) s_))) )))
    
    
    (loop
       :for s in states ;;probability of getting to final state
       :for trans =  (transition-probability M (state2id M s) 0 smooth) 
       :for vito = (aref viterbi (state2id M s) (- obs 1))
       :for p = (+ trans vito)
       :do (cond( (or (> p  (aref viterbi (state2id M s) obs ))
		      (= (aref viterbi (state2id M s) obs ) 0))
		  (setf (aref viterbi (state2id M s) obs) p)
		  (setf (aref backtrace (length states) obs) s))))

    (let(( tags  (list  ) ) ;;my backtracer looks at the most plausible earlier element in the viterbi matrix
	 ( probs (make-array (list num-st))))
      
      (loop :for timestep from 0 to (- obs 1) :do
	 (loop :for s in states :do
	    (setf (aref probs ( - (state2id M s) 1 )) (aref viterbi (state2id M s) (- obs timestep))))
	 (loop :for s from 0 to (- num-st 2)
	    :for prob1 = (aref probs s)
	    :for prob2 = (aref probs (+ s 1))
	    :do (if (> (aref probs s) (aref probs (+ s 1)))
		    (push  (nth s states) tags)
		    (push  (nth (+ s 1) states) tags)))
	 )


    
    #|(loop
       with tags = (hmm-states M)
       with final = (state2id M(aref backtrace num-st obs))
       with result = (list (elt tags final))
       for i from (- obs 1) downto 1
       for state = (state2id M (aref backtrace final i)) then (state2id M (aref backtrace state i))
       ;;:do (print tags)
       ;;:do (print result)
	 
       :do (push (elt tags state) result)
       finally (return result))
    |#
    

    tags)
    ))



;;(Viterbi eisner '("1" "1" "3" "3" "3" "3" "1" "1" "1" "1"))
;;(format t "I got stuck here for a while (4a). It seems that my probabilities are 
;;incorrect ~% or that my backtracing methods are incorrect. Perhaps both? ~%")

(defun evaluate-hmm (hmm file)
  (with-open-file (stream file :direction :input)
    (loop
       with total = 0 with correct = 0
       with forms with states
       for line = (read-line stream nil)
       for tab = (position #\tab line)
       for form = (subseq line 0 tab)
       for state = (and tab (subseq line (+ tab 1)))
       while line
       when (and form state) do
	 (push form forms)
	 (push state states)
       else do
	 (loop
	    for gold in (nreverse states)
              for state in (viterbi hmm (nreverse forms))
	    do (incf total)
	    when (string= gold state) do (incf correct))
	 (setf forms nil) (setf states nil)
       finally (return (float (/ correct total))))))

(print (evaluate-hmm eisner "~/Documents/inf4820/Lisp/3a/eisner.tt"))

(defparameter M (read-corpus "wsj.tt" 45))
(print M)
(defparameter wsj (train-hmm M))
(print (viterbi wsj '("No" "," "it" "was" "n’t" "Black" "Monday" ".")))
(print (viterbi wsj '("No" "," "it" "was" "n't" "Black" "Monday" ".")))
(print (evaluate-hmm wsj "~/Documents/inf4820/Lisp/3a/test.tt"))

;;I am not able to evaluate since I return my path of states as an array instead of a lisp.
;;I am sorry for not being able to fix this as I have 10 minutes to deliver and need to
;;comment on my code
