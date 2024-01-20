(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
signature TOML_VALUE =
sig
  datatype value =
    STRING of string (* UTF-8 encoded *)
  | INTEGER of IntInf.int
  | FLOAT of real
  | BOOL of bool
  | DATETIME of string (* 2024-01-12T19:20:21[.123]+09:00 *)
  | LOCAL_DATETIME of string (* 2024-01-12T19:20:21[.123] *)
  | DATE of string (* 2024-01-12 *)
  | TIME of string (* 19:20:21.99999 *)
  | ARRAY of value list
  | TABLE of (string * value) list
  type table = (string * value) list
  structure Integer:
  sig
    type int = IntInf.int
    val + : int * int -> int
    val * : int * int -> int
    val fromInt: Int.int -> int
    val fromString: string -> int option
  end
  val string: string -> value
  val integer: Integer.int -> value
  val float: string -> value
  val bool: bool -> value
  val datetime: string -> value
  val localDatetime: string -> value
  val date: string -> value
  val time: string -> value
  val array: value list -> value
  val subtable: table -> value
  val table: (string * value) list -> table
end
