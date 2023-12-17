(defpackage clpp
  (:use cl)
  (:export clpp-type
	   debug-mode dump-value
	   find-id location pop-front push-back repl))

(in-package clpp)

(define-symbol-macro version 1)

(defvar debug-mode nil)
