# bank-ocr-kata

Common Lisp solution to user stories 1-3 at http://codingdojo.org/cgi-bin/index.pl?KataBankOCR

## Dependencies

This system requires [SBCL](http://sbcl.org), as it uses some non-portable SBCL extensions in the tests. It also requires the [FiveAM](http://common-lisp.net/project/fiveam/) test framework. The easiest way to get FiveAM is to install [Quicklisp](http://quicklisp.org) and use `(ql:quickload 'bank-ocr-kata)` to load this system; Quicklisp will automatically download and compile the dependencies.