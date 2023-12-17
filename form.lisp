(defpackage clpp-forms
  (:use cl clpp)
  (:export compile-forms
	   emit-id-lisp emit-lisp
	   location
	   new-id new-lit new-pair))

(in-package clpp-forms)

(defstruct (form (:conc-name nil))
  (location (error "Missing location") :type location))

(defstruct (id (:include form))
  (name (error "Missing name") :type keyword))

(defun new-id (loc name)
  (make-id :location loc :name name))

(defmethod print-object ((f id) out)
  (write-string (symbol-name (id-name f)) out))

(defmethod emit-id-lisp (id clpp-type val args ns out)
  (cons val out))

(defmethod emit-lisp ((f id) args ns out)
  (let ((v (or (find-id ns (id-name f))
	       (error "Unknown id: ~a" f))))
    (emit-id-lisp (id-name f) (first v) (rest v) args ns out)))

(defstruct (lit (:include form))
  (type (error "Missing type"))
  (value (error "Missing value")))

(defun new-lit (loc type value)
  (make-lit :location loc :type type :value value))

(defmethod print-object ((f lit) out)
  (let ((v (lit-value f)))
    (dump-value (clpp-type v) v out)))

(defmethod emit-lisp ((f lit) args ns out)
  (cons (lit-value f) out))

(defstruct (pair (:include form))
  (left (error "Missing left"))
  (right (error "Missing right")))

(defun new-pair (loc left right)
  (make-pair :location loc :left left :right right))

(defmethod print-object ((f pair) out)
  (let ((l (pair-left f))
	(r (pair-right f)))
    (dump-value (clpp-type l) l out)
    (write-char #\: out)
    (dump-value (clpp-type r) r out)))
  
(defmethod emit-lisp ((f pair) args ns out)
  (cons `(cons ,(first (emit-lisp (pair-left f) args ns nil))
	       ,(first (emit-lisp (pair-right f) args ns nil)))
	out))

(defun compile-forms (in ns)
  (let (out)
    (tagbody
     next
       (let ((f (pop-front in)))
	 (when f
	   (setf out (emit-lisp f in ns out))
	   (go next))))
    (setf out (nreverse out))
    
    (compile nil `(lambda ()
                    (declare (optimize (debug ,(if debug-mode 3 0))
                                       (speed ,(if debug-mode 0 3))
                                       (safety ,(if debug-mode 0 3))))
                    ,@out))))

