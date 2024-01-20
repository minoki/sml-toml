(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
signature PARSE_TOML =
sig
  type value
  type table
  type path = string list
  val parse: (char, 'strm) StringCvt.reader -> 'strm -> table
end
