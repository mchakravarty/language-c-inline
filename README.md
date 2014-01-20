Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types. The wiki on GitHub details the [motivation](https://github.com/mchakravarty/language-c-inline/wiki/Motivation) for this approach.

Building
--------

To build the library, just use `cabal install` as usual. To build the proof of concept example, do the following

* Execute `cd tests/objc/concept; make`.
* Now run the demo executable with `./InlineObjC`.

To build the more complex Haskell interpreter app:

* Execute `cd tests/objc/app; make`.
* Now `open -a HSApp.app`.

I tested it with Haskell Platform 2013.2.0.0 (which includes GHC 7.6.3) and it requires the latest version of `language-c-quote`, which is 0.7.6.

Status
======

The library is still in its early stages. At the moment automatic marshalling support is limited to strings and boxed Haskell values represented as stable pointers in C land. However, it is quite easy to add more types, and I do welcome pull request!
