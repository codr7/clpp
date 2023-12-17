(in-package clpp)

(defun repl (ns &key (in *standard-input*) (out *standard-output*))
  (flet ((say (spec &rest args)
           (apply #'format out spec args)
           (finish-output out)))
    (let ((buf (make-string-output-stream)))
      (tagbody
       next
         (say "  ")
         (let ((line (read-line in nil)))
           (when line
             (if (string= line "")
                 (progn
                   (setf line (get-output-stream-string buf))
                   (restart-case
                       (let* ((forms (read-forms (make-string-input-stream line)
						 (new-location "repl")))
                              (imp (clpp-forms:compile-forms forms ns))
                              (result (funcall imp)))
			 (dump-value (clpp-type-of result) result out)
			 (terpri out))
                     (ignore ()
                       :report "Ignore condition.")))
                 (write-string line buf))
             (go next)))))))


