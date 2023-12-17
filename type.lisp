(in-package clpp)

(defgeneric clpp-type (lisp-type val))

(defclass clpp-type ()
  ((name :initarg :name :reader name)))

(defmethod dump-value (clpp-type val out)
  (format out "~a" val))

(defclass func-type (clpp-type)
  ())

(defmethod dump-value ((_ func-type) val out)
  (format out "(func ~a)" val))

(defvar func-type (make-instance 'prim-type :name "Func"))

(defmethod clpp-type ((_ function) val)
  func-type)

(defclass int-type (clpp-type)
  ())

(defvar int-type (make-instance 'int-type :name "Int"))

(defmethod clpp-type ((_ integer) val)
  int-type)

(defclass nil-type (clpp-type)
  ())

(defmethod dump-value ((_ nil-type) val out)
  (format out "_"))

(defvar nil-type (make-instance 'nil-type :name "Nil"))

(defmethod clpp-type (lisp-type val)
  (case val
    ((nil) nil-type)
    ((0 1) int-type)
    (otherwise
     (error "Unknown type: ~a" val))))

(defun clpp-type-of (val)
  (clpp-type (type-of val) val))
