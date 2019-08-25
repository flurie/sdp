# SDP

This is a library for simple data processing.

## Reasons

The purpose of SDP is to provide the most comfortable interface for working with
different data structures in the same namespace, while maintaining sufficiently
high quality code and speed of work so that it can be use in practice, and
translation programs from old and more specialized libraries did not cause
serious problems.
SDP classes not keen to summarize all possible data structures - only the
simplest of them. However, such structures are required in 80-95% cases.

## Functionality

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

But matrices, bitmaps, stream, dictionaries and prefix trees will also be added.

## Versioning

SDP and SDP-derived libraries must follow of the Haskell community versioning
principles and this restrictions:

In the version number a.b.c.d:
* d is the patch number: only bugfixes and code improvements. Also may be
changed valid dependency versions. Documentation changes are not subject to
versioning.
* c is internal library version number, used to mark changes in the API (only
extension, older code must compile anyway) and code improvements.
* b is the number of the stable version of SDP. Deprecated changes cannot be
removed until the next stable version.
* a is reserved (always 0) - necessary in case of release of a new edition of
SDP.

## Differences from other similar projects

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

## Derivative work

SDP permits any derivative works. If you want to create some tool on the basis
of SDP, here is a list of simple rules that will help to quickly find it,
determine what it does, and with which versions of SDP it's compatible:

### Extensions

SDP extensions (libraries that adds new features to SDP) must follow naming rule
sdp-%extensionname% (e.g. sdp-io or sdp-binary) and versioning rules.

### Wrappers

SDP wrappers (library that provides SDP functionality for an existing library)
must follow one of naming rules
* sdp4%libraryname% - for libraries that is good in itself and wrapper are
needed only for conveniece.
* sdp2%libraryname% - for libraries with limited functionality and/or bad API
(a compromise between the Last Chinese Warnings to the author and fork).

## Contributing

For details of the process for submitting pull requests, please read
[https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md](CONTRIBUTING.md).

## License

SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.
