(defun isFirst (charList char)
  (cond ((equal (car charList) char) T)
        (T nil)
	)
  )

(defun isDigit (char)
  (cond ((member char (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) T)
	)
  )

(defun isPositiveDigit (char)
  (cond ((and (not (equal char #\0)) (isDigit char)) T)
	)
  )

(defun isLowercaseEqual (a b)
  (cond ((null a) nil)
        ((equal (string-downcase (concatenate 'string (list a)))
		(concatenate 'string (list b))))
	)
  )



(defun listToString (rawList)
  (reduce (lambda (a b) (concatenate 'string a b)) rawList)
  )

(defun join (&rest args)
  (let ((stringList (mapcar (lambda (arg) (car arg)) args)))
    (listToString stringList))
  )

(defun trimWhiteSpace (stringText)
  (if (not (stringp stringText)) nil)
  (string-trim '(#\Space #\Linefeed #\Return #\Tab) stringText)
  )



(defun read-list-from (input-stream)
  (let ((e (read-line input-stream nil 'eof)))
    (unless (eq e 'eof)
      (cons e (read-list-from input-stream))))
  )

(defun jsonread (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-does-not-exist :error)
    (jsonparse (formatInput (read-list-from in))))
  )

(defun jsondump (JSON filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format out "~a" (inverseParse JSON)) filename)
  )



(defun keyParameterError ()
  (error "no keys have been called"))

(defun keyNullArrayError ()
  (error "last array is null"))

(defun keyFormatError ()
  (error "keys not in the correct format"))

(defun keyObjectError ()
  (error "key not found in object"))

(defun keyAccessError ()
  (error "last element cannot be accessed"))

(defun outOfBoundError ()
  (error "index out of bound"))

(defun syntaxError ()
  (error "syntax error"))
