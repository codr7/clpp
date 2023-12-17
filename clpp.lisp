(defpackage clpp
  (:use cl)
  (:export clpp-type clpp-type-of
	   find-id location pop-front push-back repl))

(in-package clpp)

(define-symbol-macro version 1)
