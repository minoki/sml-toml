(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
signature TOML_PARSE_ERROR =
sig
  datatype error =
    UNEXPECTED of {encountered: string, expected: string}
  | PREFIX_ZERO
  | INVALID_UNICODE_SCALAR
  | INVALID_DATE
  | INVALID_TIME
  | DUPLICATE_KEY of string list
  val toString: error -> string
  exception ParseError of error
end
