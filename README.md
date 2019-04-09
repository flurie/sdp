# SDP

That is it?
--------------------------------------------------------------------------------
Simple Data Processing library and my sandbox for working with language
extensions and data structures. Although I expect that SDP will be useful for my
more serious projects. SDP created under the impact of containers, it's not
based on collections (dead in 2010), collections-api (maybe dead in 2010) and
EdisonAPI (alive for 2018).

Why is SDP needed?
--------------------------------------------------------------------------------
The purpose of SDP is to provide work with various data structures in the same
namespace, keeping a sufficiently high quality of the code and speed of work so
that it can be used to write something practically valuable, and migration from
old and more specialized libraries doesn't cause problems.

What SDP provides?
--------------------------------------------------------------------------------
Currently, there are only three structures in SDP (lists, arrays and unrolled
linked lists). But in the near future, vectors, matrices, trees, sequences,
dictionaries, bitmaps and prefix trees will also be added.

What distinguishes SDP from similar projects?
--------------------------------------------------------------------------------
* Internal consistency. Unfortunately, not all libraries are consistent, even in
the Haskell Platform.
* Maximum functionality with minimal dependencies. SDP requires only the most
necessary and commonly used packages. This is one of the reasons why I refused
to use some existing libraries as dependencies (for example, containers, in
which there is a lot of duplicate code and an array that relies on poorly
designed Ix class).
* Orientation to other libraries. SDP is unlikely to be used by itself, but it
can become a bridge between other libraries and structures. SDP doesn't give its
own implementations of structures an advantage over external ones.
You can add general classes implementations for any data structures and they
will be able to work perfectly with native ones and between themselves. If other
developers do the same, then writing the code will be a little easier (if not,
think about using other solutions).
* Good extensibility. SDP is based on type classes that provide the simplest
interfaces for working with different data structures and reduce code
duplication. SDP will not require you to use qualified import when working with
different structures in the same namespace.

License.
--------------------------------------------------------------------------------
SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.

