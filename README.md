# Simple Data Processing

It is a library for simple data processing. `sdp` is inspired by `array`,
`vector`, `bytestring`, `containers` and `repa`. `sdp` is focused on efficiency
and openness.

## tl; dr

`sdp` is designed to combine the main `Haskell Platform` libraries and their
functionality for more comfortable work and better code readability.

In addition to the base classes and algorithms, `sdp` implements the simplest
data structures (arrays and unrolled lists) to replace the obsolete `array`
library.

## Motivation

The purpose of `sdp` is to provide the most comfortable interface for working
with various data structures in one namespace without qualified imports, keeping
a sufficiently high quality of the code and speed of work.

## Functionality

`sdp` provides basic structures and operations on them.

### Structures

All predefined structures are based on **pseudo-primitive** types `SArray#`,
`SBytes#`, `STArray#`, `STBytes#`, `IOArray#`, `IOBytes#` that securely
encapsulate real primitives.

`sdp` uses **templates** to define more complex structures:
* `AnyBorder` adds explicit boundaries of arbitrary type.
* `AnyChunks` defines an unrolled list with structure elements.

Based on pseudo-primitives and templates, the following are defined:
* immutable arrays `Array` and `Bytes`
* mutable arrays `STArray` and `STBytes`, `IOArray` and `IOBytes`
* immutable unrolled lists `Unlist` and `Ublist`
* mutable unrolled lists `STUnlist` and `STUblist`, `IOUnlist` and `IOUblist`
* immutable unrolled lists with explicit boundaries `Unrolled` and `ByteList`
* mutable expanded lists with explicit boundaries `STUnrolled` and `STByteList`,
`IOUnrolled` and `IOByteList`

### Classes

`sdp` provides a lot of conversion functions, powerful abstraction data for
writing generalized algorithms, implements the most popular operations including
selection, splitting and sorting. With `sdp` list functions are not overlap
their counterparts for other structures.

`Nullable` is a service class of structures with null values.

`Bordered` is a class of structures with borders and finite number of elements.

`Estimate` is a service class for efficiently estimating the length of a
structure. Allows to express such conditions as:
```
xs .<  ys -- structure xs is shorter than structure ys?
es .>   5 -- is structure longer than 5 elements?
es .== 10 -- The length of the structure is 10?
```
and calculate them in a finite time (in the first example at least one of the
structures must be finite). It makes sense to use `Estimate` in cases where
length calculation is more difficult than `O(1)` and you don't need to know the
exact size.

`Unboxed` is a service class that simplifies interacting with data stored in
`ByteArray#` and `MutableArray#`. Used in containers that based on
`SBytes#`, `STBytes#` or `IOBytes#`.

`Shape` is a service class for dimension operations and finite-dimensional
index transformations.

`Index` is a service class that generalizes enumeration and membership
operations interval. It is an improved version of `Ix`.

`IFold` is a service class to fold indexed sequences. Provides
`Foldable`-like functions for structures with value type constraints.

`Linear` is a class of linear structures that generalizes the standard list
functions.

`Split` is a split structure class that extends `Linear`.

`Indexed` is a class of indexed structures that generalizes read and modify
operations immutable structures.

`Shaped` is a class of operations on structures generalized by the type of
index. Provides safe change of range and index type, fast extraction of
subsequences.

`Map` is a class of operations with dictionaries.

`Set` is a class of operations on sets.

`Zip` is a class that generalizes element-wise union of structures.

`Scan` is a class of convolutions with intermediate values.

`Sort` is a sorting class for immutable structures.

`BorderedM`, `LinearM`, `SplitM`, `IndexedM`, `IFoldM`, `SortM` - classes of
operations on mutable containers.

## Versions

`sdp` follow [Haskell Package Versioning Policy](https://pvp.haskell.org). To
simplify the search for derivative components, I propose the following rules:
* The `MAJOR` version of the derivative must match the smallest `MAJOR` version
of `sdp` with which it's compatible.
* `sdp` extensions should be called `sdp-%extensionname%`, e.g. `sdp-io` or
`sdp-quickcheck`.
* `sdp` wrappers should be called `sdp4%libraryname%`, e.g. `sdp4text`.
* Some wrappers may be called `sdp2%libraryname%`, e.g. `sdp2binary`.

## Differences from other similar projects

* **Internal consistency.** Unfortunately, not all Haskell libraries are
self-consistent, even in the `Haskell Platform`. The current version
`sdp-0.2` is much better than the preliminary ones.
* **Not another rebase.** `sdp` works **with** `base`, generalizes and extends
it.
* **Maximum functionality with minimum size**. The `sdp` is comparable in size
to `containers`, and it only requires 3 simple dependencies (including `base`).
`sdp` and its dependencies do not require `array`, `containers` or `vector`, it
is completely independent of them.
* **Good extensibility.** Since SDP is based on typeclasses, I can easily add
new function without updating derived components. For SDP compatibility, it is
enough to implement several open and universal interfaces.
* **Targeting other libraries.** Although SDP was created as `array`
replacement, its main purpose was to eliminate name conflicts with other
libraries and (partially) between them. The library generalizes standard
functions and thus hides many qualified imports. SDP wrappers also improves code
readability. For example, you can work with text and binary streams (`Text` and
`ByteString`) in one module without unnecessary qualifiers.

## Using the SDP category

The SDP category can be used for:
* `sdp` type classes and extensions.
* Types based on pseudo-primitives and `sdp` templates.
* Wrapper modules, to avoid a more than 3-level hierarchy. For example,
`SDP.ByteString.Lazy` instead of `Data.ByteString.Lazy.SDP`.

If other categories (`Control`, `Foreign`, `System`, etc.) can be used, they
must be used.

## Contributing
For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License
`sdp` is free software, you can redistribute it and/or modify it under the terms
of the BSD3 license. `sdp` is distributed in the hope that it will be useful,
but without any warranty, without even the implied warranty of merchantability
or fitness for a particular purpose. See the BSD3 license for more details.

