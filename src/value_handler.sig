(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
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
