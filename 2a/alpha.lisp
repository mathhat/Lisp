(remove-if-not #'alphanumericp (string-downcase "!Faku."))
(remove-if #'evenp '(1 2 3 4 5 6 7 8 9 10 11))
(if (evenp 2) (print "wag"))

(defun func (i)
  (if (evenp i)
      i
      0))


(defun filt (sequence func)
  (loop
     :for i :in sequence
     :if (function i)
     :do (print i)))

(filt '(1 2 3 4 5 6 0) #'func)
