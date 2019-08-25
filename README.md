# SDP

This is a library for simple data processing.

## Reasons

The purpose of SDP is to provide the most comfortable interface for working with
different data structures in the same namespace, keeping sufficiently high
quality code and speed of work. So that it can be use in practice, and
transition programs from old and more specialized libraries did not cause
serious problems.

Classes in SDP keep a balance between universality and efficiency. They may not
be suitable for working with some data structures, but with the most common ones
are perfectly combined.

## Functionality

SDP provides a wide range of options for data conversion, a powerful abstraction
for generalized programming, and the most common operations, including (but not
limited to) filtering, splitting, and sorting.

Now SDP provides only 6 standard data structures for use in programs that are
not very demanding on speed and efficiency:

- standard lists ([])
- binary trees   (BinTree)

- lazy    boxed  arrays (Array, STArray)
- strict unboxed arrays (Bytes, STBytes)

- lazy   unrolled linked lists (Unrolled, STUnrolled)
- strict unrolled linked lists (ByteList, STByteList)

And two service structures (may be infinite):

- lazy   unrolled linked lists (Unlist, STUnlist)
- strict unrolled linked lists (Ublist, STUblist)

But matrices, bitmaps, streams, dictionaries and prefix trees will also be added
in new versions.

SDP provides the following classes:

- Bordered/BorderedM - for getting the bounds, list of indices and associations
(index, element).
- Linear - for separate elements, create structure from list or Foldable, left
and right views of structure. Also contains some generalized list functions
(concat, intersperse, filter, partition, reverse and nubBy) and support three
generic patterns.
- Indexed/IndexedM - for create structure from list of associations or other
Indexed/IndexedM, for rewriting or updating it, for reading and writing (4 and 2
resp. for both) and for searching indices of elements by predicate.
- Sort/SortM - for sort data structures. Also SDP contain timSort algorithm
implementation for any IndexedM.
- Set - for standard set operations.

- Zip and Scan - for overloaded zips and scans (only suitable for structures
generalized by value type).

- Estimate - for lazy comparings by length.
- Unboxed - overloaded by value type interface for ByteArray creating, filling,
reading and writing.
- Index - for index types.
- IndexEQ - for long trivial Index instances (it’s better to lose a little
performance than to look for errors in a hundred lines of monotonous code).

## Versioning

SDP and SDP-derived libraries must follow of the Haskell community versioning
principles and this restrictions:

For the version a.b.c.d
* d is the patch number: only bugfixes and code improvements. Also may be
changed valid dependency versions.
* c is the internal library version number, used to mark changes in the API
(only extension, older code must compile anyway) and big code improvements.
* b is the number of the stable version of SDP. Deprecated changes cannot be
removed until the next stable version.
* a is reserved (always 0) - necessary in case of release a new edition of SDP.

## Differences from other similar projects

* Internal consistency. Unfortunately, not all libraries are self-consistent,
even in the Haskell Platform.
* Maximum functionality with the minimum number of dependencies. SDP requires
only the most necessary and commonly used packages. This is one of the reasons
for which I refused to use some libraries as dependencies (for example,
containers, in which a lot of duplicate code and array, which relies on a poorly
designed Ix class).
* Orientation to other libraries. SDP is unlikely to be used by itself, but it
can become a bridge between other libraries and structures. SDP doesn't give its
own implementations of structures an advantage over external ones (except,
perhaps, voluntary-compulsory use of Index class). You can add general classes
implementations for any data structures and they will be able to work perfectly
with native ones and between themselves. If other developers do the same, then
writing the code will be a little easier.
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
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License

SDP is free software, you can redistribute it and/or modify it under the
terms of the BSD3 license.
SDP is distributed in the hope that it will be useful, but without any
warranty, without even the implied warranty of merchantability or fitness for
a particular purpose. See the BSD3 license for more details.

