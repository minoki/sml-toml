(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
structure CustomTomlValue =
struct
  datatype value =
    STRING of string (* UTF-8 encoded *)
  | INTEGER of IntInf.int
  | FLOAT of string
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
structure CustomParseToml = ParseToml(CustomTomlValue)
structure Main:
sig
  val main: 'a -> 'b
end =
struct
  fun jsChar #"\"" = "\\\""
    | jsChar #"\\" = "\\\\"
    | jsChar #"\r" = "\\r"
    | jsChar #"\n" = "\\n"
    | jsChar c =
        if c < #" " then
          if Char.ord c < 16 then "\\u000" ^ Int.fmt StringCvt.HEX (Char.ord c)
          else "\\u00" ^ Int.fmt StringCvt.HEX (Char.ord c)
        else
          String.str c
  fun dumpString s =
    "\"" ^ String.translate jsChar s ^ "\""
  fun intToString (i: IntInf.int) =
    if i < 0 then "-" ^ IntInf.toString (~i) else IntInf.toString i
  fun dumpTable table =
    "{"
    ^
    String.concatWith ","
      (List.map (fn (k, v) => dumpString k ^ ":" ^ dumpValue v) table) ^ "}"
  and dumpValue (CustomTomlValue.STRING s) =
        "{\"type\":\"string\",\"value\":" ^ dumpString s ^ "}"
    | dumpValue (CustomTomlValue.INTEGER i) =
        "{\"type\":\"integer\",\"value\":\"" ^ intToString i ^ "\"}"
    | dumpValue (CustomTomlValue.FLOAT "+nan") =
        "{\"type\":\"float\",\"value\":\"nan\"}"
    | dumpValue (CustomTomlValue.FLOAT "-nan") =
        "{\"type\":\"float\",\"value\":\"nan\"}"
    | dumpValue (CustomTomlValue.FLOAT s) =
        "{\"type\":\"float\",\"value\":\"" ^ s ^ "\"}"
    | dumpValue (CustomTomlValue.BOOL b) =
        "{\"type\":\"bool\",\"value\":\"" ^ Bool.toString b ^ "\"}"
    | dumpValue (CustomTomlValue.DATETIME dt) =
        "{\"type\":\"datetime\",\"value\":\"" ^ dt ^ "\"}"
    | dumpValue (CustomTomlValue.LOCAL_DATETIME dt) =
        "{\"type\":\"datetime-local\",\"value\":\"" ^ dt ^ "\"}"
    | dumpValue (CustomTomlValue.DATE d) =
        "{\"type\":\"date-local\",\"value\":\"" ^ d ^ "\"}"
    | dumpValue (CustomTomlValue.TIME t) =
        "{\"type\":\"time-local\",\"value\":\"" ^ t ^ "\"}"
    | dumpValue (CustomTomlValue.ARRAY xs) =
        "[" ^ String.concatWith "," (List.map dumpValue xs) ^ "]"
    | dumpValue (CustomTomlValue.TABLE table) = dumpTable table
  fun fail s =
    ( TextIO.output (TextIO.stdErr, s ^ "\n")
    ; OS.Process.exit OS.Process.failure
    )
  fun main _ =
    let
      val table =
        CustomParseToml.parse
          (ValidateUtf8.validatingReader TextIO.StreamIO.input1)
          (ValidateUtf8.mkValidatingStream (TextIO.getInstream TextIO.stdIn))
      val _ =
        ParseToml.parse (ValidateUtf8.validatingReader TextIO.StreamIO.input1)
          (ValidateUtf8.mkValidatingStream (TextIO.getInstream TextIO.stdIn))
    in
      print (dumpTable table ^ "\n");
      OS.Process.exit OS.Process.success
    end
    handle
      TomlParseError.ParseError e => fail (TomlParseError.toString e)
    | ValidateUtf8.InvalidUtf8 => fail "invalid UTF-8";
end
