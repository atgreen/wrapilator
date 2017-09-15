# wrapilator

The wrapilator is a tool that creates a thin Common Lisp veneer over
verilator output, allowing for the creation of digital logic test code
in Common Lisp.

## Installation

Wrapilator has been tested on sbcl, and requires quicklisp.  Run
`make` to generate a binary executable for wrapilator.

## Usage

    $ wrapilator
    wrapilator - copyright (C) 2017 Anthony Green <green@moxielogic.com>

    Usage: wrapilator [-h|--help] [-d|--directory OBJ_DIR] module-name

    Available options:
      -h, --help               print this help text
      -d, --directory OBJ_DIR  verilator output directory (default: obj_dir)

## Author

Wrapilator was written by Anthony Green.

email    : green@moxielogic.com

linkedin : http://linkedin.com/in/green

twitter  : [@antgreen](https://twitter.com/antgreen)


