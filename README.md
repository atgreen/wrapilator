# wrapilator

The wrapilator is a tool that creates a thin Common Lisp veneer over
[verilator](https://www.veripool.org/wiki/verilator) output, allowing
for the creation of digital logic test code in Common Lisp.
Wrapilator creates C stubs for the C++ verilator output, and a Common
Lisp package that talks to those stubs via [CFFI](https://common-lisp.net/project/cffi/).

## Installation

Wrapilator has been developed and tested with [sbcl](http://sbcl.org)
on Linux ([Fedora](https://getfedora.org)), and requires the use of
[quicklisp](https://www.quicklisp.org) to download dependencies.  Run
`make` to generate a stand-alone binary executable for wrapilator.
The build process requires the
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

Imagine a simple verilog module, counter.v, that looks like this:

    module counter (clk_i, rst_i, counter_o);
      input rst_i, clk_i;
      output [7:0] counter_o;
      reg [7:0] counter;
    
      assign counter_o = counter;
       
      always @(posedge clk_i) begin
         if (rst_i) 
            counter <= 0;
         else
    	counter <= counter + 1;
      end
     endmodule

Run the following commands to generate everything:

    $ verilator -cc counter.v
    $ wrapilator counter

This generates the verilator code, the Lisp wrapper, and associated
makefiles.

The generated lisp code looks like this:

    ;;; verilated-counter.lisp
    ;;;
    ;;; created on Sun Sep 17 08:07:27 2017 by wrapilator.
    
    (ql:quickload :cffi)
    
    (defpackage #:verilated-counter
      (:use #:cl #:cffi)
      (:export #:counter-new
               #:counter-eval 
               #:counter-set-clk-i
               #:counter-set-rst-i
               #:counter-get-counter-o))
    
    (in-package #:verilated-counter)
    
    (define-foreign-library libcounter
      (t (:default "/home/green/git/example/obj_dir/libcounter")))
    
    (use-foreign-library libcounter)
    
    (defcfun "counter_new" :pointer)
    (defcfun "counter_eval" :void (c :pointer)) 
    (defcfun "counter_set_clk_i" :void (obj :pointer) (val :uint)) 
    (defcfun "counter_set_rst_i" :void (obj :pointer) (val :uint))  
    (defcfun "counter_get_counter_o" :uint (obj :pointer)) 

Note that all the getter and setter functions replace '_' with '-', as
per the CFFI rules.  Use `counter-new` to create a new `counter`
object, and `counter-eval` to evaluate the verilated modules.

Now you can build everything in the verilator output directory:

    $ (cd obj_dir; make -f Makefile.wrap)
    M32=-fPIC make -f Vcounter.mk
    make[1]: Entering directory '/home/green/git/example/obj_dir'
    /usr/bin/perl /usr/share/verilator/bin/verilator_includer -DVL_INCLUDE_OPT=include Vcounter.cpp > Vcounter__ALLcls.cpp
    g++  -I.  -MMD -I/usr/share/verilator/include -I/usr/share/verilator/include/vltstd -DVL_PRINTF=printf -DVM_COVERAGE=0 -DVM_SC=0 -DVM_TRACE=0 -Wno-char-subscripts -Wno-sign-compare -Wno-uninitialized -Wno-unused-but-set-variable -Wno-unused-parameter -Wno-unused-variable   -fPIC    -c -o Vcounter__ALLcls.o Vcounter__ALLcls.cpp
    /usr/bin/perl /usr/share/verilator/bin/verilator_includer -DVL_INCLUDE_OPT=include Vcounter__Syms.cpp > Vcounter__ALLsup.cpp
    g++  -I.  -MMD -I/usr/share/verilator/include -I/usr/share/verilator/include/vltstd -DVL_PRINTF=printf -DVM_COVERAGE=0 -DVM_SC=0 -DVM_TRACE=0 -Wno-char-subscripts -Wno-sign-compare -Wno-uninitialized -Wno-unused-but-set-variable -Wno-unused-parameter -Wno-unused-variable   -fPIC    -c -o Vcounter__ALLsup.o Vcounter__ALLsup.cpp
          Archiving Vcounter__ALL.a ...
    ar r Vcounter__ALL.a Vcounter__ALLcls.o Vcounter__ALLsup.o
    ar: creating Vcounter__ALL.a
    ranlib Vcounter__ALL.a
    make[1]: Leaving directory '/home/green/git/example/obj_dir'
    g++ -fPIC -shared -I/usr/share/verilator/include -I/usr/share/verilator/include/vltstd -I. -o libcounter.so wrapper.c -Wl,--whole-archive Vcounter__ALL.a -Wl,--no-whole-archive

Now that the libraries and Lisp wrapper have been generated, you can
create a test file that looks like so:

    (load "obj_dir/verilated-counter.lisp")
    (use-package :verilated-counter)
    
    ;;; Create a new counter
    (defvar *counter* (counter-new))
    
    (defun tick-clock ()
      (counter-set-clk-i *counter* 1)
      (counter-eval *counter*)
      (counter-set-clk-i *counter* 0)
      (counter-eval *counter*))
    
    ;;; Hold reset high for CYCLES
    (defun reset-cycles (cycles)
      (counter-set-rst-i *counter* 1)
      (loop for i from 1 to cycles do
           (tick-clock))
      (counter-set-rst-i *counter* 0))
    
    (reset-cycles 5)
      
    (loop for i from 1 to 10 do
          (tick-clock))
    
    (format t "counter value is ~A~%" (counter-get-counter-o *counter*))
    (exit)

And now you can run your code like so:

    $ sbcl --noinform --load test.lisp
    To load "cffi":
      Load 1 ASDF system:
        cffi
    ; Loading "cffi"
    .
    counter value is 10

But even more importantly, you can fire up a slime session in emacs
and explore your module in a REPL.

Another early example of Common Lisp test code using wrapilator output
can be found at
https://raw.githubusercontent.com/atgreen/moxie-cores/master/bench/mox125/test.lisp.
This is test code for the mox125 core.  The Makefile for invoking
verilator and wrapilator are found here:
https://github.com/atgreen/moxie-cores/blob/master/bench/mox125/Makefile.

## Author

Wrapilator was written by Anthony Green.

* email    : green@moxielogic.com
* linkedin : http://linkedin.com/in/green
* twitter  : [@antgreen](https://twitter.com/antgreen)
