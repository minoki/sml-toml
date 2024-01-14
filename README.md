# sml-toml

This is a parser of [TOML v1.0.0](https://toml.io/en/v1.0.0) for Standard ML.

## Usage

Currently, compilers compatible with MLB are supported.
`toml.mlb` provides:

```sml
signature TOML_VALUE = sig
  datatype value = STRING of string (* UTF-8 encoded *)
                 | INTEGER of IntInf.int
                 | FLOAT of string (* without underscores *)
                 | BOOLEAN of bool
                 | DATETIME of string (* 2024-01-12T19:20:21[.123]+09:00 *)
                 | LOCAL_DATETIME of string (* 2024-01-12T19:20:21[.123] *)
                 | DATE of string (* 2024-01-12 *)
                 | TIME of string (* 19:20:21[.99999] *)
                 | ARRAY of value list
                 | TABLE of (string * value) list
  type table = (string * value) list
end
structure TomlValue :> TOML_VALUE

signature PARSE_TOML = sig
  type value = TomlValue.value
  type table = (string * TomlValue.value) list
  type path = string list
  exception UnexpectedEndOfInput
  exception UnknownEscapeChar
  exception InvalidString
  exception UnexpectedChar
  exception InvalidNumber
  exception InvalidDateTime
  exception DuplicateKey of path
  val parse : (char, 'strm) StringCvt.reader -> 'strm -> table
end
structure ParseToml :> PARSE_TOML

signature VALIDATE_UTF8 = sig
  exception InvalidUtf8
  type 'strm validating_stream
  val mkValidatingStream : 'strm -> 'strm validating_stream
  val validatingReader : (char, 'strm) StringCvt.reader -> (char, 'strm validating_stream) StringCvt.reader
end
structure ValidateUtf8 :> VALIDATE_UTF8
```

Note that `ParseToml.parse` does not do UTF-8 validation.
If you want to check for invalid UTF-8 sequences, combine with `ValidateUtf8.validatingReader`.
See `test/decoder.sml` for example.

## Test

This program passes [toml-test](https://github.com/toml-lang/toml-test):

```sh-session
$ cd test
$ make
$ env GOBIN="$(pwd)" go install github.com/toml-lang/toml-test/cmd/toml-test@latest
$ ./toml-test ./decoder
toml-test [./decoder]: using embedded tests: 415 passed,  0 failed
```

## License

[MIT License](LICENSE)
