#SDP

That is it?
--------------------------------------------------------------------------------
Simple Data Processing library and my sandbox for working with language
extensions and data structures. Although I expect that sense will come out of
SDP and it can be used as a component in more serious projects.

Why is it needed?
--------------------------------------------------------------------------------
The purpose of SDP is, firstly, to save acceptable working quality(in comparison
with older and specialized libraries) with greater functionality, and secondly,
to allow the joint use of various structures and libraries.

What SDP provides?
--------------------------------------------------------------------------------
Currently, there are only two structures in SDP (lists and arrays), and most of
the code requires testing. But in the near future, vectors, matrices, trees,
sequences, dictionaries, bitmaps, unrolled lists and prefix trees will also be
added.

What distinguishes SDP from other libraries?
--------------------------------------------------------------------------------
* Internal consistency. I would be sincerely happy if all the libraries were
consistent. At least in Haskell. At least in the Haskell Platform.
* Maximum functionality with minimal number of dependencies. SDP requires only
the most necessary and commonly used packages. This is one of the reasons why I
refused to use some existing libraries as dependencies (for example, containers,
in which there is a lot of duplicate code and an array that relies on poorly
designed Ix class).
* Orientation to other libraries. SDP is unlikely to ever become a good library
by itself. But it can be a good interface for the collaboration of other
libraries and will develop primarily in this direction. SDP does not benefit its
own implementation of structures.
* It has good extensibility. SDP is based on type classes that provide the
simplest interfaces for working with different data structures and reduce code
duplication. SDP will not require you to use qualified import when working with
different structures in the same namespace.
* Damn Russian developer with poor knowledge of English.

License.
--------------------------------------------------------------------------------
SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.

