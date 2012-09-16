`Data.Stringable` is a set of type classes for interacting with any of the
usual Haskell string-like types: `String`, `Text`, `Text.Lazy`, `ByteString`,
`ByteString.Lazy` and `Filesystem.Path`.

At the moment only conversions are supported, but I intend to support the full
set of operations allowed for all these types.  This will allow library code
that works with string-like types to use a type class, rather than decide on a
single type or set of types.
