;;;; wrapilator.asd

(asdf:defsystem #:wrapilator
  :description "Wrap verilator output with a thin Common Lisp veneer"
  :author "Anthony Green <green@moxielogic.com>"
  :license "GPLv2"
  :depends-on (#:cl-template
	       #:unix-opts
	       #:inferior-shell
	       #:cl-ppcre
	       #:com.gigamonkeys.pathnames
	       #:local-time)
  :serial t
  :components ((:file "package")
               (:file "wrapilator")))

