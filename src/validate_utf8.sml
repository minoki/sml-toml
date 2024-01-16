(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
signature VALIDATE_UTF8 =
sig
  exception InvalidUtf8
  type 'strm validating_stream
  val mkValidatingStream: 'strm -> 'strm validating_stream
  val validatingReader: (char, 'strm) StringCvt.reader
                        -> (char, 'strm validating_stream) StringCvt.reader
end
structure ValidateUtf8 :> VALIDATE_UTF8 =
struct
  exception InvalidUtf8
  datatype state =
    START
  | MID_1_OF_2
  | MID_1_OF_3_E0
  | MID_1_OF_3_ED
  | MID_1_OF_3_OTHER
  | MID_2_OF_3
  | MID_1_OF_4_F0
  | MID_1_OF_4_OTHER
  | MID_1_OF_4_F4
  | MID_2_OF_4
  | MID_3_OF_4
  type 'strm validating_stream = 'strm * state
  fun mkValidatingStream strm = (strm, START)
  (*
   * 1-byte: 0aaa aaaa / U+0000 - U+007F
   * 2-byte: 110a aaaa 10bb bbbb / U+0080 - U+07FF / 0b00010 <= aaaaa
   * 3-byte: 1110 aaaa 10bb bbbb 10cc cccc / U+0800 - U+D7FF, U+E000 - U+FFFF / 0b100000 <= aaaa_bbbbbb < 0b1101_100000, 0b1110_000000 <= aaaa_bbbbbb
   * 4-byte: 1111 0aaa 10bb bbbb 10cc cccc 10dd dddd / U+10000 - U+10FFFF / 0b10000 <= aaa_bbbbbb < 0b100_010000
   *)
  fun nextState (START, c) =
        if c < #"\u0080" then
          START
        else if c < #"\u00E0" then
          if #"\u00C2" <= c then MID_1_OF_2 else raise InvalidUtf8
        else if c = #"\u00E0" then
          MID_1_OF_3_E0
        else if c = #"\u00ED" then
          MID_1_OF_3_ED
        else if c < #"\u00F0" then
          MID_1_OF_3_OTHER
        else if c = #"\u00F0" then
          MID_1_OF_4_F0
        else if c < #"\u00F4" then
          MID_1_OF_4_OTHER
        else if c = #"\u00F4" then
          MID_1_OF_4_F4
        else
          raise InvalidUtf8
    | nextState (MID_1_OF_2, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" then START
        else raise InvalidUtf8
    | nextState (MID_1_OF_3_E0, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" andalso #"\u00A0" <= c1 then
          MID_2_OF_3
        else
          raise InvalidUtf8
    | nextState (MID_1_OF_3_ED, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" andalso c1 < #"\u00A0" then
          MID_2_OF_3
        else
          raise InvalidUtf8
    | nextState (MID_1_OF_3_OTHER, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" then MID_2_OF_3
        else raise InvalidUtf8
    | nextState (MID_2_OF_3, c2) =
        if #"\u0080" <= c2 andalso c2 < #"\u00C0" then START
        else raise InvalidUtf8
    | nextState (MID_1_OF_4_F0, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" andalso #"\u0090" <= c1 then
          MID_2_OF_4
        else
          raise InvalidUtf8
    | nextState (MID_1_OF_4_OTHER, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" then MID_2_OF_4
        else raise InvalidUtf8
    | nextState (MID_1_OF_4_F4, c1) =
        if #"\u0080" <= c1 andalso c1 < #"\u00C0" andalso c1 < #"\u0090" then
          MID_2_OF_4
        else
          raise InvalidUtf8
    | nextState (MID_2_OF_4, c2) =
        if #"\u0080" <= c2 andalso c2 < #"\u00C0" then MID_3_OF_4
        else raise InvalidUtf8
    | nextState (MID_3_OF_4, c3) =
        if #"\u0080" <= c3 andalso c3 < #"\u00C0" then START
        else raise InvalidUtf8
  fun go (START, NONE) = NONE
    | go (_, NONE) = raise InvalidUtf8
    | go (state, SOME (c, strm)) =
        SOME (c, (strm, nextState (state, c)))
  fun validatingReader getc (strm, state) =
    go (state, getc strm)
end
