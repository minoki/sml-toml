(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
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
and dumpValue (TomlValue.STRING s) =
      "{\"type\":\"string\",\"value\":" ^ dumpString s ^ "}"
  | dumpValue (TomlValue.INTEGER i) =
      "{\"type\":\"integer\",\"value\":\"" ^ intToString i ^ "\"}"
  | dumpValue (TomlValue.FLOAT "+nan") =
      "{\"type\":\"float\",\"value\":\"nan\"}"
  | dumpValue (TomlValue.FLOAT "-nan") =
      "{\"type\":\"float\",\"value\":\"nan\"}"
  | dumpValue (TomlValue.FLOAT s) =
      "{\"type\":\"float\",\"value\":\"" ^ s ^ "\"}"
  | dumpValue (TomlValue.BOOL b) =
      "{\"type\":\"bool\",\"value\":\"" ^ Bool.toString b ^ "\"}"
  | dumpValue (TomlValue.DATETIME dt) =
      "{\"type\":\"datetime\",\"value\":\"" ^ dt ^ "\"}"
  | dumpValue (TomlValue.LOCAL_DATETIME dt) =
      "{\"type\":\"datetime-local\",\"value\":\"" ^ dt ^ "\"}"
  | dumpValue (TomlValue.DATE d) =
      "{\"type\":\"date-local\",\"value\":\"" ^ d ^ "\"}"
  | dumpValue (TomlValue.TIME t) =
      "{\"type\":\"time-local\",\"value\":\"" ^ t ^ "\"}"
  | dumpValue (TomlValue.ARRAY xs) =
      "[" ^ String.concatWith "," (List.map dumpValue xs) ^ "]"
  | dumpValue (TomlValue.TABLE table) = dumpTable table
fun main () =
  let
    val table =
      ParseToml.parse (ValidateUtf8.validatingReader TextIO.StreamIO.input1)
        (ValidateUtf8.mkValidatingStream (TextIO.getInstream TextIO.stdIn))
  in
    print (dumpTable table ^ "\n")
  end;
fun fail s =
  (TextIO.output (TextIO.stdErr, s ^ "\n"); OS.Process.exit OS.Process.failure)
val () =
  main ()
  handle
    TomlParseError.ParseError e => fail (TomlParseError.toString e)
  | ValidateUtf8.InvalidUtf8 => fail "invalid UTF-8";
