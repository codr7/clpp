(defpackage clpp-forms
  (:use cl clpp)
  (:export compile-forms emit-identifier-lisp emit-lisp new-identifier))

(in-package clpp-forms)

(defstruct form
  (location (error "Missing location") :type location))

(defstruct (identifier (:include form))
  (name (error "Missing name") :type keyword))

(defun new-identifier (loc name)
  (make-identifier :location loc :name name))

(defmethod print-object ((id identifier) out)
  (write-string (symbol-name (identifier-name id)) out))

(defmethod emit-identifier-lisp (id clpp-type val args ns out)
  (cons val out))

(defmethod emit-lisp ((id identifier) args ns out)
  (let ((v (or (find-id ns (identifier-name id))
	       (error "Unknown identifier: ~a" id))))
    (emit-identifier-lisp (identifier-name id) (first v) (rest v) args ns out)))

(defvar debug-mode nil)
(defvar emit-speed 0)

(defun compile-forms (in ns)
  (let (out)
    (tagbody
     next
       (let ((f (pop-front in)))
	 (when f
	   (setf out (emit-lisp f in ns out))
	   (go next))))
    (setf out (nreverse out))
    
    ;(when debug-mode
      ;(push `(format t "~a~%" ,(format nil "~a" out)) out))

    (compile nil `(lambda ()
                    (declare (optimize (debug ,(if debug-mode 3 0))
                                       (speed ,(floor emit-speed 3))
                                       (safety ,(floor (- 9 emit-speed) 3))))
                    ,@out))))

