(in-package clpp)

(defstruct (position (:conc-name nil) (:constructor new-position))
	   (source (error "Missing source") :type string)
	   (line 1 :type integer)
	   (column 0 :type integer))

(defmethod clone ((p position))
  (copy-structure p))

(defmethod print-object ((p position) out)
  (format out "~a@~a:~a" (source p) (line p) (column p)))
