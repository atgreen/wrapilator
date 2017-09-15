;;;; wrapilator.asd

(asdf:defsystem #:wrapilator
  :description "Wrap verilator output with a thin Common Lisp veneer"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on (#:cl-template
	       #:unix-opts
	       #:inferior-shell
	       #:uiop
	       #:cl-ppcre
	       #:com.gigamonkeys.pathnames
	       #:local-time)
  :serial t
  :components ((:file "package")
               (:file "wrapilator")))

