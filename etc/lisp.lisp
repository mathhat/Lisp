(print "What's your name")
(+ 5 2)
(+ 7 3)
(defun ! (n)
  (if (= n 0)
      1
      (* n (!(- n 1)))))
