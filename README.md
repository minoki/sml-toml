# sml-toml

This is a parser of [TOML v1.0.0](https://toml.io/en/v1.0.0) for Standard ML.

## Usage

Currently, compilers compatible with MLB are supported.
`toml.mlb` provides:

```sml
signature TOML_PARSE_ERROR = sig
  datatype error = UNEXPECTED of { encountered : string, expected : string }
                 | PREFIX_ZERO
                 | INVALID_UNICODE_SCALAR
                 | INVALID_DATE
                 | INVALID_TIME
                 | DUPLICATE_KEY of string list
  val toString : error -> string
  exception ParseError of error
end
structure TomlParseError :> TOML_PARSE_ERROR

signature TOML_VALUE_HANDLER = sig
  type value
  type table
  structure Integer : sig
    type int
    val + : int * int -> int
    val * : int * int -> int
    val fromInt : Int.int -> int
    val fromString : string -> int option
  end
  val string : string -> value
  val integer : Integer.int -> value
  val float : string -> value
  val bool : bool -> value
  val datetime : string -> value
  val localDatetime : string -> value
  val date : string -> value
  val time : string -> value
  val array : value list -> value
  val subtable : table -> value
  val table : (string * value) list -> table
end

signature PARSE_TOML = sig
  type value
  type table
  type path = string list
  val parse : (char, 'strm) StringCvt.reader -> 'strm -> table
end

functor ParseToml (Handler : TOML_VALUE_HANDLER) :> PARSE_TOML
                                                    where type value = Handler.value
                                                    where type table = Handler.table

signature TOML_VALUE = sig
  datatype value = STRING of string (* UTF-8 encoded *)
                 | INTEGER of IntInf.int
                 | FLOAT of string (* without underscores *)
                 | BOOL of bool
                 | DATETIME of string (* 2024-01-12T19:20:21[.123]+09:00 *)
                 | LOCAL_DATETIME of string (* 2024-01-12T19:20:21[.123] *)
                 | DATE of string (* 2024-01-12 *)
                 | TIME of string (* 19:20:21[.99999] *)
                 | ARRAY of value list
                 | TABLE of (string * value) list
  type table = (string * value) list
  structure Integer : sig
    type int = IntInf.int
    val + : int * int -> int
    val * : int * int -> int
    val fromInt : Int.int -> int
    val fromString : string -> int option
  end
  val string : string -> value
  val integer : Integer.int -> value
  val float : string -> value
  val bool : bool -> value
  val datetime : string -> value
  val localDatetime : string -> value
  val date : string -> value
  val time : string -> value
  val array : value list -> value
  val subtable : table -> value
  val table : (string * value) list -> table
end
structure TomlValue :> TOML_VALUE

structure ParseToml :> PARSE_TOML
                       where type value = TomlValue.value
                       where type table = TomlValue.table

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
