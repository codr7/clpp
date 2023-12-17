(in-package clpp)

(defun new-std ()
  (let ((ns (new-namespace)))
    (bind-id ns :_ nil)
    (bind-id ns :|VERSION| version)
    ns))

(defvar std (new-std))

	    
