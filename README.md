THIS IS AN EXPERIMENTAL PROOF OF CONCEPT IMPLEMENTATION
=======================================================

It is not really useful for anything at the moment.


Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types. The wiki on GitHub details the [motivation](wiki/Motivation) for this approach.

Building
--------

This is currently just an experiment with a dumb build system. To compile it, you need the Haskell Platform 2012.4.0.0 (with GHC 7.4.2) and my fork of `language-c-quote` (for the Objective-C support). Specifically,

* Clone my fork of [`language-c-quote`](https://github.com/mchakravarty/language-c-quote/tree/inline-objc) and select the `inline-objc` branch.
* In the cloned repository, run `cabal install`.
* Clone this repository (`language-c-inline`).
* In the cloned repository, run `make`.
* Now run the demo executable with `./InlineObjC`.

Contents
--------
The proof of concept has two main components:

* `TestInlineObjC.hs`: This is an example of what a user writes to use the Inline C & Objective-C library.
* `InlineObjC.hs`: This is a mock up of the Template Haskell that turns inline Objective-C code into a helper Objective-C file (here, it is `TestInlineObjC_objc.m`) and vanilla Haskell that invokes the Objective-C code via the regular Haskell C FFI. It works for this example, but is otherwise rather incomplete.
