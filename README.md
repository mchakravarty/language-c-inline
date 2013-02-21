THIS IS AN EXPERIMENTAL PROOF OF CONCEPT IMPLEMENTATION
=======================================================

It is not really useful for anything at the moment.


Inline C &amp; Objective-C in Haskell
=====================================

This library uses Template Haskell and `language-c-quote`, a quasi-quotation library for C-like languages, to provide inline C and Objective-C in Haskell. It extracts the C/Objective-C code automatically, when compiling the Haskell program, and generates marshalling code for common types.

Building
--------

This is currently just an experiment with a dumb build system. To compile it, you need the Haskell Platform 2012.4.0.0 (with GHC 7.4.2) and my fork of `language-c-quote` (for the Objective-C support).

