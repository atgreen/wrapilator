# wrapilator

The wrapilator is a tool that creates a thin Common Lisp veneer over
[verilator](https://www.veripool.org/wiki/verilator) output, allowing
for the creation of digital logic test code in Common Lisp.
Wrapilator creates C stubs for the C++ verilator output, and a Common
Lisp package that talks to those stubs via [CFFI](https://common-lisp.net/project/cffi/).

## Installation

Wrapilator has been tested on [sbcl](http://sbcl.org), and I strongly
recommend the use of [quicklisp](https://www.quicklisp.org) to
download dependencies.  Run `make` to generate a binary executable for
wrapilator.  The build process requires the
[`buildapp`](https://www.xach.com/lisp/buildapp/) program from
quicklisp.

## Usage

    $ wrapilator
    wrapilator - copyright (C) 2017 Anthony Green <green@moxielogic.com>

    Usage: wrapilator [-h|--help] [-d|--directory OBJ_DIR] module-name

    Available options:
      -h, --help               print this help text
      -d, --directory OBJ_DIR  verilator output directory (default: obj_dir)

## Example

An early example of Common Lisp test code using wrapilator output can
be found at
https://raw.githubusercontent.com/atgreen/moxie-cores/master/bench/test.lisp.
This is test code for the mox125 core's insruction cache.  The
Makefile for invoking verilator and wrapilator are found here:
https://raw.githubusercontent.com/atgreen/moxie-cores/master/bench/Makefile.

## Author

Wrapilator was written by Anthony Green.

* email    : green@moxielogic.com
* linkedin : http://linkedin.com/in/green
* twitter  : [@antgreen](https://twitter.com/antgreen)


