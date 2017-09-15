;;; wrapilator.lisp

;;; Copyright (C) 2017  Anthony Green <green@moxielogic.com>
;;; Distrubuted under the terms of the MIT license.

(in-package #:wrapilator)

(defvar *directory* "obj_dir")

(defun read-file-into-string (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;;; Read all of the template files into strings.
(defvar *Makefile-template* (read-file-into-string "Makefile.clt"))
(defvar *wrapper.c-template* (read-file-into-string "wrapper.c.clt"))
(defvar *package.lisp-template* (read-file-into-string  "package.lisp.clt"))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :directory
   :description "verilator output directory (default: obj_dir)"
   :arg-parser #'identity
   :short #\d
   :long "directory"
   :meta-var "OBJ_DIR"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition)) 
 (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun abspath
       (path-string)
   (uiop:unix-namestring
    (uiop:merge-pathnames*
     (uiop:parse-unix-namestring path-string))))

(defun usage ()
  (opts:describe
   :prefix "wrapilator - copyright (C) 2017 Anthony Green <green@moxielogic.com>"
   :suffix "Distributed under the terms of MIT License"
   :usage-of "wrapilator"
   :args     "module-name"))

(defun create-wrapper (module-name)
  ;; Sanity checks
  (let ((header (format nil "~A/V~A.h" *directory* module-name)))
    (if (not (com.gigamonkeys.pathnames:file-exists-p header))
	(format t "fatal: verilator output header file ~A does not exist~%" header)
	(progn
	  (with-open-file (stream (format nil "~A/Makefile.wrap" *directory*)
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
	    (format stream (funcall (cl-template:compile-template *Makefile-template*)
				    (list :module-name module-name))))
	  (let ((input-lines (inferior-shell:run/lines
			      (format nil "grep -h VL_IN ~A" header)))
		(output-lines (inferior-shell:run/lines
			      (format nil "grep -h VL_OUT ~A" header))))
	    (with-open-file (stream (format nil "~A/verilated-~A.lisp" *directory* module-name)
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
	      (format stream (funcall (cl-template:compile-template *package.lisp-template*)
				      (list :module-name module-name :directory (abspath *directory*) :input-lines input-lines :output-lines output-lines))))
	    (with-open-file (stream (format nil "~A/wrapper.c" *directory*)
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
	      (format stream (funcall (cl-template:compile-template *wrapper.c-template*)
				      (list :module-name module-name :input-lines input-lines :output-lines output-lines)))))))))
	
(defun main (args)
  (multiple-value-bind (options free-args)
		       (handler-case
			   (handler-bind ((opts:unknown-option #'unknown-option))
			     (opts:get-opts))
			 (opts:missing-arg (condition)
					   (format t "fatal: option ~s needs an argument!~%"
						   (opts:option condition)))
			 (opts:arg-parser-failed (condition)
						 (format t "fatal: cannot parse ~s as argument of ~s~%"
							 (opts:raw-arg condition)
							 (opts:option condition))))
		       (when-option (options :help)
				    (usage))
		       (when-option (options :directory)
				    (setf *directory* (getf options :directory)))
		       (if (not (eq 1 (length free-args)))
			   (usage)
			   (progn
			     (create-wrapper (car free-args))
			     (format t "Done.~%")))))
