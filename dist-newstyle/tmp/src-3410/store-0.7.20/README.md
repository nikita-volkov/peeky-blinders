# store

The 'store' package provides efficient binary serialization. There are
a couple features that particularly distinguish it from most prior
Haskell serialization libraries:

* Its primary goal is speed. By default, direct machine
  representations are used for things like numeric values (`Int`,
  `Double`, `Word32`, etc) and buffers (`Text`, `ByteString`,
  `Vector`, etc). This means that much of serialization uses the
  equivalent of `memcpy`.

  We have plans for supporting architecture independent
  serialization - see [#36](https://github.com/fpco/store/issues/36)
  and [#31](https://github.com/fpco/store/issues/31). This plan makes
  little endian the default, so that the most common endianness has no
  overhead.

  - Another way that the serialization behavior can vary is if
    integer-simple is used instead of GHC's default of using
    GMP. `Integer` serialized with the `integer-simple` flag enabled
    are not compatible with those serialized without the flag enabled.

* Instead of implementing lazy serialization / deserialization
  involving multiple input / output buffers, `peek` and `poke` always
  work with a single buffer. This buffer is allocated by asking the
  value for its size before encoding. This simplifies the encoding
  logic, and allows for highly optimized tight loops.

* `store` can optimize size computations by knowing when some types
  always use the same number of bytes.  This allows us to compute the
  byte size of a `Vector Int32` by just doing `length v * 4`.

It also features:

* Optimized serialization instances for many types from base, vector,
  bytestring, text, containers, time, template-haskell, and more.

* TH and GHC Generics based generation of Store instances for
  datatypes.

* TH generation of testcases.

* Utilities for streaming encoding / decoding of Store encoded
  messages, via the `store-streaming` package.

## Gotchas

Store is best used for communication between trusted processes and
local caches.  It can certainly be used for other purposes, but the
builtin set of instances have some gotchas to be aware of:

* Store's builtin instances serialize in a format which depends on
  machine endianness.

* Store's builtin instances trust the data when deserializing. For
  example, the deserialization of `Vector` will read the vector's
  length from the first 8 bytes. It will then allocate enough memory
  to store all the elements. Malicious or malformed input could cause
  allocation of large amounts of memory.  See [issue #122][].

* Serialization may vary based on the version of datatypes. For
  example, `Text` serialized from `text < 2` will not be compatible
  with `Text` from `text >= 2`, because the internal representation
  switched from UTF-16 to UTF-8.

[issue #122]: https://github.com/fpco/store/issues/122

## Blog posts

* [Initial release announcement](https://www.fpcomplete.com/blog/2016/05/store-package)
* [Benchmarks of the prototype](https://www.fpcomplete.com/blog/2016/03/efficient-binary-serialization)
* [New 'weigh' allocation benchmark package](https://www.fpcomplete.com/blog/2016/05/weigh-package),
  created particularly to aid optimizing `store`.
