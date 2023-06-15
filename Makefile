wrapilator: *.asd *.lisp *.clt Makefile
	ocicl install
	sbcl --dynamic-space-size 2560 --eval "(require 'asdf)" --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :wrapilator) (sb-ext:quit))"

clean:
	-rm -rf wrapilator systems *~
