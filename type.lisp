(in-package clpp)

(defgeneric clpp-type (val))

(defclass clpp-type ()
  ((name :initarg :name :reader name)))

(defmethod dump-value (clpp-type val out)
  (format out "~a" val))

(defclass func-type (clpp-type)
  ())

(defmethod dump-value ((_ func-type) val out)
  (format out "~a" val))

(defvar func-type (make-instance 'func-type :name "Func"))

(defmethod clpp-type ((_ function))
  func-type)

(defclass int-type (clpp-type)
  ())

(defvar int-type (make-instance 'int-type :name "Int"))

(defmethod clpp-type ((_ integer))
  int-type)

(defclass nil-type (clpp-type)
  ())

(defmethod dump-value ((_ nil-type) val out)
  (format out "_"))

(defvar nil-type (make-instance 'nil-type :name "Nil"))

(defclass pair-type (clpp-type)
  ())

(defmethod dump-value ((_ pair-type) val out)
  (let ((l (first val))
	(r (rest val)))
    (dump-value (clpp-type l) l out)
    (write-char #\: out)
    (dump-value (clpp-type r) r out)))

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod clpp-type ((_ cons))
  pair-type)

(defmethod clpp-type (val)
  (case val
    ((nil) nil-type)
    ((0 1) int-type)
    (otherwise
     (error "Unknown type: ~a" val))))
