(in-package clpp)

(defstruct (namespace (:conc-name nil))
  (parent-namespace nil :type (or null namespace))
  (bindings (make-hash-table) :type hash-table))

(defun new-namespace (&optional parent-ns)
  (make-namespace :parent-namespace parent-ns))

(defun find-id (ns id)
  (let ((v (gethash id (bindings ns))))
    (or v (let ((pns (parent-namespace ns)))
	    (and pns (find-id pns id))))))

(defun bind-id (ns id val)
  (setf (gethash id (bindings ns)) (cons (clpp-type-of val) val)))

(defun (setf find-id) (val ns id)
  (bind-id ns id val))
