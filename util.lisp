(in-package clpp)

(defun char-digit (c)
  (- (char-int c) (char-int #\0)))
