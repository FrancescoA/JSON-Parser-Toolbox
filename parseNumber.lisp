(defun parseNumber (charList)
  (let* ((minusSign (parseMinusSign charList))
         (integer (parseInteger (cadr minusSign)))
         (floatDecimal (parseFloatDecimal (cadr integer)))
         (exponent (parseExponent (cadr floatDecimal)))
         (numberString (join minusSign integer floatDecimal exponent)))
    (list (read-from-string numberString) (cadr exponent)))
  )

(defun parseNumberDemo(string)
  (cond ((stringp string) (parseNumber (coerce string 'list)))
        )
  )



(defun parseMinusSign (charList)
  (cond ((isFirst charList #\-) (list "-" (cdr charList)))
        (T (list nil charList))
	)
  )

(defun parseSign (charList Acc)
  (cond ((isFirst charList #\+)
	 (parseExponentDigit (cdr charList) (append Acc (list #\+))))
        ((isFirst charList #\-)
	 (parseExponentDigit (cdr charList) (append Acc (list #\-))))
        (T (syntaxError))
	)
  )



(defun parseDigit (charList Acc)
  (cond ((isDigit (car charList))
	 (parseDigit (cdr charList) (append Acc (list (car charList)))))
        (T (list (concatenate 'string Acc) charList))
	)
  )

(defun parseInteger (charList)
  (cond ((isFirst charList #\0) (list "0" (cdr charList)))
        ((isPositiveDigit (car charList))
	 (parseDigit (cdr charList) (list (car charList))))
        (T (syntaxError))
	)
  )



(defun parseFloatDecimal (charList)
  (cond ((isFirst charList #\.) (parseDigit (cdr charList) (list #\.)))
	(T (list nil charList))
	)
  )



(defun parseExponentDigit (charList Acc)
  (cond ((isDigit (car charList))
	 (parseDigit (cdr charList) (append Acc (list (car charList)))))
        (T (syntaxError))
	)
  )

(defun parseExponent (charList)
  (cond ((isLowercaseEqual (car charList) #\e)
	 (parseSign (cdr charList) (list #\e)))
        (T (list nil charList))
	)
  )