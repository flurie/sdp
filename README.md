# SDP

That is it?
--------------------------------------------------------------------------------
This is a library for simple data processing and my sandbox for working with
language extensions. However, I hope that sense will come of it, and I can use
it in more serious projects.
SDP was created under the influence of containers, it is not based on
collections (abandoned 2010), collections-api (most likely abandoned),
EdisonAPI (alive as of 2018) or any such library - it's independent project.

Why is SDP needed?
--------------------------------------------------------------------------------
The purpose of SDP is to provide the most comfortable interface for working with
different data structures in the same namespace, while maintaining sufficiently
high quality code and speed of work so that it can be use in practice, and
translation programs from old and more specialized libraries did not cause
serious problems.
SDP classes not keen to summarize all possible data structures - only the
simplest of them. However, such structures are required in 80-95% cases.

What SDP provides?
--------------------------------------------------------------------------------
Currently there are only six data structures in SDP:

- standard lists ([])
- binary trees   (BinTree)

- lazy   immutable  boxed  arrays (Array, STArray)
- strict immutable unboxed arrays (Bytes, STBytes)

- lazy   unrolled linked lists (Unrolled, STUnrolled)
- strict unrolled linked lists (ByteList, STByteList)

And two service structures (may be infinite):

- lazy   unrolled linked lists (Unlist, STUnlist)
- strict unrolled linked lists (Ublist, STUblist)

But matrices, bitmaps, sequences, dictionaries, and prefix trees will also be
added.

How does SDP differ from other libraries?
--------------------------------------------------------------------------------
* Internal consistency. Unfortunately, not all libraries are consistent, even in
the Haskell Platform.
* Maximum functionality with the minimum number of dependencies. SDP requires
only the most necessary and commonly used packages. This is one of the reasons
for which I refused to use some libraries as dependencies (for example,
containers, in which a lot of duplicate code and array, which relies on a poorly
designed Ix class).
* Orientation to other libraries. SDP is unlikely to be used by itself, but it
can become a bridge between other libraries and structures. SDP doesn't give its
own implementations of structures an advantage over external ones (except,
perhaps, own implementation of n-dimensional indices). You can add general
classes implementations for any data structures and they will be able to work
perfectly with native ones and between themselves. If other developers do the
same, then writing the code will be a little easier.
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

