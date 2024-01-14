(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
signature TOML_VALUE = sig
  datatype value = STRING of string (* UTF-8 encoded *)
                 | INTEGER of IntInf.int
                 | FLOAT of string (* without underscores *)
                 | BOOLEAN of bool
                 | DATETIME of string (* 2024-01-12T19:20:21[.123]+09:00 *)
                 | LOCAL_DATETIME of string (* 2024-01-12T19:20:21[.123] *)
                 | DATE of string (* 2024-01-12 *)
                 | TIME of string (* 19:20:21.99999 *)
                 | ARRAY of value list
                 | TABLE of (string * value) list
  type table = (string * value) list
end
