THIS IS STILL PROOF OF CONCEPT
==============================

It is not really useful for anything at the moment.


Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types. The wiki on GitHub details the [motivation](https://github.com/mchakravarty/language-c-inline/wiki/Motivation) for this approach.

Building
--------

To build the library, just use `cabal install` as usual. To build the proof of concept example, do the following

* Execute `cd tests/objc/concept; make`.
* Now run the demo executable with `./InlineObjC`.

I tested it with Haskell Platform 2012.4.0.0 (which includes GHC 7.4.2) and it requires the latest version of `language-c-quote`, which is 0.7.1.

Contents
--------
The proof of concept has two main components:

* `tests/objc/concept/InlineObjC.hs`: This is an example of what a user writes to use the Inline C & Objective-C library.
* `Language/C/Inline/ObjC.hs`: This is a mock up of the Template Haskell that turns inline Objective-C code into a helper Objective-C file (here, it is `TestInlineObjC_objc.m`) and vanilla Haskell that invokes the Objective-C code via the regular Haskell C FFI. It works for this example, but is otherwise rather incomplete.
