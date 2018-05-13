# Common Lisp sample bot

Lisp is a homoiconic language that was there from the beginning and
still has a fresh perspective to offer.

## Environment requirements

Install [SBCL](http://www.sbcl.org/platform-table.html) common lisp compiler and make sure it's on your path.

Install [QuickLisp](https://www.quicklisp.org/beta/#installation) and add quicklisp to your SBCL init file (by running `(ql:add-to-init-file)` after loading quicklisp.lisp in SBCL).

## Building

I've included a build script in build.lisp which compiles the bot code and dumps the Lisp runtime and compiled program into a standalone executable. This script only works for the SBCL compiler.

```
sbcl --load build.lisp
```

## Running 

The build script produces an executable in the current directory called bot which is run with

```
./bot
```