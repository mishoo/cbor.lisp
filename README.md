# CBOR encoder/decoder for Common Lisp
### _Mihai Bazon <mihai.bazon@gmail.com>_

See [RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949) - “Concise Binary
Object Representation”.

This package is a fast implementation for Common Lisp. Note that I only tested
it on SBCL, and it might in fact use some non-standard features, like
`WITH-SLOTS` on a structure instance; patches for other CL implementations are
welcome.

For now the only exported functions are `encode` and `decode`.

```lisp
(cbor:encode value) => sequence-of-bytes
(cbor:decode sequence-of-bytes) => value
```

Where `sequence-of-bytes` is of type `(simple-array (unsigned-byte 8) 1)`.

You can throw pretty much any Lisp value at `encode` (except circular data) and
it should do what you'd reasonably expect. Some dynamic variables control what
happens in the usual CL ambiguities:

- `*jsown-semantics*` — bind this to `T` if you want the encoder/decoder to work
  similarly to JSOWN (which is the fastest JSON parser for CL that I
  tried). Currently, that means maps will decode to `(:OBJ . alist)`, and the
  encoder will encode a similar object to a map. Arrays will always be parsed as
  lists, thus the empty array will decode to `NIL`; conversely, `NIL` will be
  encoded as empty array. If you bind this variable to `T`, you probably also
  want to bind `*symbol-to-string*` and `*string-to-symbol*` to `NIL` (see
  below).

- `*array-format*` (default `:array`) — bind this to `:list` if you prefer the
  array decoder to create lists instead of arrays.

- `*symbol-to-string*` is called by the encoder when a symbol is
  encountered. The default implementation lowercases the name and replaces
  dashes with underscores. If you pass `NIL`, `symbol-name` will be called
  instead.

- `*string-to-symbol*` is called by the decoder when reading map keys. The
  default is the reverse of `*symbol-to-string*`, and interns the name into the
  package designated by `*symbols-package*` (defaults to `KEYWORD` package). If
  bound to `NIL`, map keys will remain as they are.

- `*symbols-package*` — see above.

- `*dictionary-format*` (default `:hash`). Bind this to `:alist` if you prefer
  the decoder for maps to create alists, or to `:plist` if you prefer plists. If
  hash tables are preferred, they are created with `:test #'EQ` if
  `*string-to-symbol*` is non-NIL, or `:test #'EQUAL` otherwise. Note that
  `*jsown-semantics*` takes precedence over this, so if that is `T` then maps
  will be decoded as `(:OBJ . alist)`.

Sequences of type `(vector (unsigned-byte 8))` will be encoded as binary — “byte
string”
([major type 2](https://datatracker.ietf.org/doc/html/rfc8949#name-major-types)).

Besides the major types, we support timestamps and bignums. As `LOCAL-TIME` is
the de-facto time manipulation library for CL, we encode a
`LOCAL-TIME:TIMESTAMP` object using the extended tag `1` and double-float
representation (precision should be at millisecond-level). Conversely, the
decoder will create `LOCAL-TIME:TIMESTAMP` objects when it encounters a datetime
value. The decoder also supports extended tag `0` (datetime as string), but
disclaimer: I didn't test that.

Bignums (extended tags `3` and `4`) should work as you'd expect.

Objects and structures are encoded as maps.

## playground.lisp

Eval `playground.lisp` (SBCL-specific) if you want some quick test
functions. `(test-decode value)` will encode and then decode and return the
value, and it'll print the binary. `(test-json "/path/to/file.json")` will
decode a JSON file using YASON/JSOWN, CBOR-encode/decode it with our library,
and print some timing/consing information. (note that it'll dump some files into
the same directory, with binaries produced for each test).

CBOR stands pretty good. Note that in timing the JSON libraries, I have included
the time spent on bytes<->text UTF8 conversion. I think that's fair; that step
does happen at some level, even though a high-level JSON library is normally not
concerned with it. But even if you discount that time, CBOR is still faster.

## License

MIT
