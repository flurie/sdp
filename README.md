# Simple Data Processing

This is a library for simple data processing. SDP is inspired by array, vector,
bytestring and partly by containers and repa. SDP focuses on efficiency and
openness.

## tl;dr

SDP is a compilation of best features of common Haskell Platform structure
libraries. It's interchangeable with array and vector (except Bundles),
almost with containers (except Graphs and Trees) and partly with the ByteString
(Bytes and Unrolled is higher level analogues of strict and lazy bytestrings).

## Reasons

The purpose of SDP is to provide the most comfortable interface for working with
different data structures in the same namespace without qualified imports,
keeping sufficiently high quality code and speed of work. So that it can be use
in practice, and rewriting programs from old and more specialized libraries
didn't cause serious problems.

## Functionality

SDP provides a lot of common data conversion functions, a powerful abstraction
for generalized programming and the most used operations, including (but not
limited to) filtering, splitting and sorting.

With SDP list functions don't overlap analogues for other structures.

SDP provides 12 common data structures (not including lists):
- immutable arrays:
[Array](https://github.com/andreymulik/sdp/blob/master/src/SDP/Array.hs) and
[Bytes](https://github.com/andreymulik/sdp/blob/master/src/SDP/Bytes.hs)
- immutable unrolled lists:
[Unlist](https://github.com/andreymulik/sdp/blob/master/src/SDP/Unrolled/Unlist.hs),
[Ublist](https://github.com/andreymulik/sdp/blob/master/src/SDP/ByteList/Ublist.hs),
[Unrolled](https://github.com/andreymulik/sdp/blob/master/src/SDP/Unrolled.hs) and
[ByteList](https://github.com/andreymulik/sdp/blob/master/src/SDP/ByteList.hs)
- mutable arrays:
[STArray](https://github.com/andreymulik/sdp/blob/master/src/SDP/Array/ST.hs) and
[STBytes](https://github.com/andreymulik/sdp/blob/master/src/SDP/Bytes.ST.hs)
- mutable unrolled lists:
[STUnlist](https://github.com/andreymulik/sdp/blob/master/src/SDP/Unrolled/ST.hs),
[STUblist](https://github.com/andreymulik/sdp/blob/master/src/SDP/ByteList/ST.hs),
[STUnrolled](https://github.com/andreymulik/sdp/blob/master/src/SDP/Unrolled.ST.hs)
and [STByteList](https://github.com/andreymulik/sdp/blob/master/src/SDP/ByteList/ST.hs)

Also SDP has pseudo-primitive types (SArray#, SBytes#, STArray# and STBytes#)
that simplifies the implementation of more complex structures. They are
protected from tampering and provide some important guarantees.

SDP provides the following classes:

- Bordered, Linear and Indexed - for common immutable operations.
- BorderedM, LinearM and IndexedM - for most common mutable operations.
- IFold and IFoldM - folds with index. For some structures may be used instead
Foldable.
- Sort and SortM - sorts for immutable and mutable structures.
- Set and Map - for set and map operations.
- Index, Unboxed, Estimate, Zip and Scan - service classes.

## Versioning

SDP follow [Haskell Package Versioning Policy](https://pvp.haskell.org).
To simplify the search for derivative components, I propose the following rules:
* The MAJOR version of the derivative must match the smallest MAJOR version of
SDP with which it's compatible.
* The MINOR version is left to the discretion of the derivative developer.
* SDP extensions should be called by the rule: sdp-%extensionname%.
* SDP wrappers should be called by one of the follow rules.
sdp4%libraryname% (for libraries that already has most of the provided by
wrapper functionality, but need a little generalization). Or sdp2%libraryname% -
for poor libraries that not only generalized, but also expanded by the wrapper.

## Differences from other similar projects

* Internal consistency. Unfortunately, not all libraries are self-consistent,
even in the Haskell Platform.
* Maximum functionality with the minimal dependencies and size. SDP requires
only the most necessary and commonly used packages. This is one of the reasons
for which I refused to use some libraries as dependencies (for example, array,
which relies on a poorly designed Ix class or containers, in which a lot of the
code duplication).
* Good extensibility. SDP is based on type classes that provide the simplest
interfaces for working with different data structures and reduce code
duplication. SDP will not requires qualified imports when work with different
structures in the same namespace.
* Orientation to other libraries. This library is interchangeable with array and
vector, however this isn't the main purpose. First of all, SDP is a way of
interacting libraries at the level of data abstraction. SDP doesn't give its own
implementations an advantage over external.

## SDP category using

SDP category must be used for:
* type classes declared in SDP and extensions
* types whose names are occupied in the Data category. For example, Array can't
be placed in the Data.Array module due to a conflict with the array package.
* types based on SDP pseudo-primitives (STUnlist, STUblist, as well as based on
them Unrolled and ByteList)
* wrapper modules, to avoid a 3-5 level hierarchy. For example, the wrapper for
Data.ByteString is SDP.ByteString, not Data.ByteString.SDP and the wrapper for
Data.ByteString.Lazy is SDP.ByteString.Lazy, not Data.ByteString.Lazy.SDP or
Data.ByteString.SDP.Lazy

SDP category mustn't be used for types from System, Control, Foreign, Test and
other important categories (for example, instances for Foreign.C types should be
in Foreign.C.SDP, not in SDP.C or SDP.Foreign.C).

## Contributing

For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License

SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.

