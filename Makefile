wrapilator: *.asd *.lisp *.clt Makefile
	buildapp --output wrapilator \
		--asdf-path `pwd`/.. \
		--asdf-tree ~/quicklisp/dists/quicklisp/software \
		--load-system wrapilator \
		--compress-core \
		--entry "wrapilator:main"

clean:
	-rm -f wrapilator
	-rm -f *~
