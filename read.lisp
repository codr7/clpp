(in-package clpp)

(defun read-identifier (in out loc)
  (let* ((fl (clone loc))
	 (s (with-output-to-string (s)
              (tagbody
               next
                 (let ((c (read-char in nil)))
                   (unless (member c (list nil #\( #\)))
		     (incf (column loc))
		     (write-char c s)
		     (go next))
		   (when c
                     (unread-char c in)))))))
    (when (zerop (length s))
      (return-from read-identifier))
    (push-back out (clpp-forms:new-identifier fl (intern s :keyword)))
    t))

(defun read-whitespace (in out loc)
  (declare (ignore out))
  
  (tagbody
   next
     (let ((c (read-char in nil)))
       (when c
         (case c
           (#\newline
            (incf (line loc))
            (setf (column loc) 0))
           (#\space
            (incf (column loc)))
	   (otherwise
            (unread-char c in)
	    (return-from read-whitespace)))
	 (go next))))
  nil)

(defun read-forms (in loc)
  (let ((out (new-deque)))
    (tagbody
     next
       (dolist (r (list #'read-whitespace #'read-identifier))
	 (when (funcall r in out loc)
	   (go next))))
    out))
