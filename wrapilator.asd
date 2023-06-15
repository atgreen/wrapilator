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
	       #:local-time)
  :serial t
  :components ((:file "package")
               (:file "wrapilator"))
  :build-operation "program-op"
  :build-pathname "wrapilator"
  :entry-point "wrapilator:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
