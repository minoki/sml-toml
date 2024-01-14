(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
structure TomlValue :> TOML_VALUE = struct
  datatype value = STRING of string (* UTF-8 encoded *)
                 | INTEGER of IntInf.int
                 | FLOAT of string (* without underscores *)
                 | BOOL of bool
                 | DATETIME of string (* 2024-01-12T19:20:21[.123]+09:00 *)
                 | LOCAL_DATETIME of string (* 2024-01-12T19:20:21[.123] *)
                 | DATE of string (* 2024-01-12 *)
                 | TIME of string (* 19:20:21.99999 *)
                 | ARRAY of value list
                 | TABLE of table
  withtype table = (string * value) list
  structure Integer = IntInf
  val string = STRING
  val integer = INTEGER
  val float = FLOAT
  val bool = BOOL
  val datetime = DATETIME
  val localDatetime = LOCAL_DATETIME
  val date = DATE
  val time = TIME
  val array = ARRAY
  val subtable = TABLE
  val table = fn xs => xs
end
structure ParseToml = ParseToml (TomlValue)
