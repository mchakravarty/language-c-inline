Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types. The wiki on GitHub details the [motivation](https://github.com/mchakravarty/language-c-inline/wiki/Motivation) for this approach.

For further motivation, you might like to watch the [YouTube video](http://www.youtube.com/watch?v=pm_WFnWqn20) (matching [slides](https://speakerdeck.com/mchakravarty/foreign-inline-code-in-haskell-haskell-symposium-2014)) of my talk at the Haskell Symposium 2014 (Gothenburg, Sweden), or look at the slides of my YOW! Lambda Jam 2014 talk [Foreign Inline Code in Haskell](https://speakerdeck.com/mchakravarty/foreign-inline-code-in-haskell).

Building
--------

To build the library, just use `cabal install` as usual from the source code directory or by installing from Hackage.

You may like to have a look at a [minimal example](tests/objc/minimal/Main.hs) of its use, which you can build as follows:

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

Status
======

**Update:** For various reasons (mostly lack of time on my side), all my recent changes have been on the `release/0.7` branch, which also hosts the version that you can find on Hackage. That version has been used in production. It is the glue between the Haskell and Swift part of [Haskell for Mac](http://haskellformac.com). For a larger open-source example of using `language-c-inline`, see [HaskellSpriteKit](https://github.com/mchakravarty/HaskellSpriteKit).

Unfortunately, I haven't been able to find the time to merge all the improvements on `release/0.7` back into `master`.

The library is in beta and focuses mostly on Objective-C. Automatic marshalling support is still somewhat limited. However, it is quite easy to add more types, and I do welcome pull request!
