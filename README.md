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
* `AnyChunks` defines an unrolled list with structure elements
* `AnyBorder` adds explicit boundaries of arbitrary type

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
`ByteArray#` and `MutableArray#`. Used in containers that based on `SBytes#`,
`STBytes#` or `IOBytes#`.

`Shape` is a service class for dimension operations and finite-dimensional index
transformations.

`Index` is a service class that generalizes enumeration and membership
operations interval. It is an improved version of `Ix`.

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

`BorderedM`, `LinearM`, `SplitM`, `IndexedM`, `SortM` - classes of operations on
mutable containers.

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

* **Not another ((re)re)base.** `sdp` works **with** `base`, generalizes and
extends it.
* **Maximum functionality with minimal size**. The `sdp` requires only 3 simple
dependencies including `base` and comparable with some common packages by size:

| package    | version  | compiler   | .so  | lines | code |
|------------|----------|------------|------|-------|------|
| sdp        | 0.2      | ghc-8.4.4  | 5.5M | 10.5k | 5.7k |
| vector     | 0.12.1.2 | ghc-8.10.2 | 4.3M | 16k   | 6.9k |
| containers | 0.6.2.1  | ghc-8.10.2 | 3.5M | 24k   | 6.5k |

* **Good extensibility.** `sdp` is a well-extensible library that allows you to
easily integrate new components, simplifies interlibrary communication. The
functionality of `sdp` is also easy to extend with the flexibility that type
classes provide.
* **Orientation to other libraries.** Originally `sdp` was created as `array`
replacement, now its main purpose was to eliminate name conflicts with other
libraries and between them. The library generalizes standard functions and thus
hides many qualified imports. `sdp` wrappers also improves code readability. For
example, you can work with text and binary streams (`Text` and `ByteString`) in
one module without unnecessary qualifiers.
* **Modern design.** `sdp` is what array would look like if it were written now.
`sdp` has a good balance between openness, reliability, and performance. `sdp`
doesn't impose significant restrictions on the implementation and doesn't go too
far with the complexity of the definitions, trying to be both understandable and
universal. But also `sdp` tries to keep up with the times, providing support for
many non-standard features of the language, if they are appropriate.

## Using the SDP category

The `SDP` category is intended for classes and structures whose names are
already taken in the `Data` category. It shouldn't be used instead of `System`,
`Control`, `Foreign`, etc.

## Contributing
For details of the process for submitting pull requests, please read
[CONTRIBUTING.md](https://github.com/andreymulik/sdp/blob/master/CONTRIBUTING.md).

## License
`sdp` is FOSS (free and open source software), you can redistribute it and/or
modify it under the terms of the BSD3 license. `sdp` is distributed in the hope
that it will be useful, but without any warranty, without even the implied
warranty of merchantability or fitness for a particular purpose. See the BSD3
license for more details.

