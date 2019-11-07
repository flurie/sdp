# Simple Data Processing

This is a library for simple data processing. SDP is inspired by array, vector,
bytestring and partly by containers and repa.

SDP focuses on efficiency and openness. Therefore, there are unsafe functions in
SDP.

## tl;dr

SDP is a compilation of best (in my humble opinion) features of array, vector,
containers and bytestring. It's completely interchangeable with the first two,
almost with the third and partly with the fourth.

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

SDP provides 12 common data structures (not including lists):
- standard list []. With SDP, list functions don't overlap analogues for other
structures.
- immutable arrays (lazy boxed Array and strict unboxed Bytes). Similar to
arrays from package array, but have more functions. SDP borrows some
features of vectors and strict bytestrings, which makes some operations in O(1)
instead of O(n).
- immutable unrolled lists (lazy boxed Unlist and Unrolled, strict unboxed
Ublist and ByteList). Ublist is a lazy bytestring analogue, that can store
values of any Unboxed type, not only Word8 and Char. Unlist - boxed version of
Ublist. ByteList and Unrolled - versions of Ublist and Unlist with explicit
bounds.
- mutable arrays and unrolled lists (STArray, STBytes, STUnrolled, STUnlist,
STByteList, STUblist).

Also SDP has pseudo-primitive types (SArray#, SBytes#, STArray# and STBytes#)
that simplifies the implementation of more complex structures. They are
protected from tampering and provide some important guarantees.

SDP provides the following classes:

- Bordered/BorderedM - for operations with bounds.
- Linear, LinearM - for structure construction and deconstruction using standard
lists or their Foldable analogues. Linear also provides useful generic patterns
and common list-like operations (filter, concat, reverse, nub, etc.).
- Indexed, IndexedM - for create structure from list of associations or other
Indexed/IndexedM, for rewriting or updating it, for elementwise reading and
writing.
- IFold, IFoldM - for folds with index, also extends Foldable on Indexed
structures (for example, Bytes and ByteList can't be Foldable, but can be IFold).
- Sort, SortM - for sort data structures. Also SDP contain timSort algorithm
implementation for all IndexedM.
- Set, Map - for standard set and map operations.
- Zip, Scan - for overloaded zips and scans (only suitable for structures
generalized by element's type).
- Estimate - for lazy comparing by length.
- Unboxed - overloaded interface for create, fill, read and write ByteArray#-
based structures.
- Index - replacement for Ix class, extendable realisation of overloaded index
type.

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
duplication. SDP will not requires qualified imports when working with different
structures in the same namespace.
* Orientation to other libraries. SDP, array and vector are essentially
interchangeable. In some cases, SDP can also replace containers and, in very few
cases, bytestring. However, this isn't its main purpose. SDP must help other
libraries interact better with each other.

## SDP category using

SDP category must be used for:
* type classes declared in SDP and extensions
* types whose names are occupied in the Data category (for example, Array can't
be placed in the Data.Array module due to a conflict with the array package)
* types similar to those already in the SDP category (for example, Bytes is
similar to Array and putting it into Data.Bytes would be counterintuitive)
* types based on pseudo-primitives SDP (STUnlist, STUblist, as well as based on
them Unrolled and ByteList)
* wrapper modules, so as not to pile up 3-5-level hierarchies (for example, the
wrapper for Data.ByteString is SDP.ByteString, not Data.ByteString.SDP and the
wrapper for Data.ByteString.Lazy is SDP.ByteString.Lazy, not
Data.ByteString.Lazy.SDP or Data.ByteString.SDP.Lazy)

SDP category mustn't be used for:
* types from Foreign and other important categories (for example, instances for
Foreign.C types should be in Foreign.C.SDP, and not in SDP.C or SDP.Foreign.C)
* exception types (like Control.Exception.SDP)
* non-library modules (must be in SDP.Internal)

Test.SDP should be used to test classes only.

## Contributing

For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License

SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.





