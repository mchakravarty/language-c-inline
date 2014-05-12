Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types. The wiki on GitHub details the [motivation](https://github.com/mchakravarty/language-c-inline/wiki/Motivation) for this approach.

For further motivation, have a look at the slides of my YOW! Lambda Jam 2014 talk [Foreign Inline Code in Haskell](https://speakerdeck.com/mchakravarty/foreign-inline-code-in-haskell).

Building
--------

To build the library, just use `cabal install` as usual from the source code directory or by installing from Hackage.

You may like to have a look at a [minimal example](blob/master/tests/objc/minimal/Main.hs) of its use, which you can build as follows:

* Execute `cd tests/objc/minimal; make`.
* Now run the demo executable with `./Minimal`.

To build the proof of concept example, do the following:

* Execute `cd tests/objc/concept; make`.
* Now run the demo executable with `./InlineObjC`.

To build an example class wrapping a Haskell record, do the following:

* Execute `cd tests/objc/record; make`.
* Now run the demo executable with `./Particle`.

To build the more complex Haskell interpreter app:

* Execute `cd tests/objc/app; make`.
* Now `open -a HSApp.app`.

I tested it with Haskell Platform 2013.2.0.0 (which includes GHC 7.6.3) and it requires the latest version of `language-c-quote`, which is currently 0.7.7.

Status
======

The library is in beta and so far focuses on Objective-C. Automatic marshalling support is still somewhat limited. However, it is quite easy to add more types, and I do welcome pull request!
