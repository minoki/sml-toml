(*
 * Copyright (c) 2024 ARATA Mizuki
 * This file is part of sml-toml.
 * See LICENSE for copyright information.
 *)
structure StringExt = struct
  fun implodeRev accum = String.implode (List.rev accum)
  open String (* There may be a built-in String.implodeRev *)
end
structure Common = struct
  (* type pos = { line : int, column : int } (* 0-based line and 0-based column (in bytes) *) *)
  type path = string list
  type key = string list (* non-empty list *)
  exception UnexpectedEndOfInput
  exception UnknownEscapeChar
  exception InvalidString
  exception UnexpectedChar
  exception InvalidNumber
  exception InvalidDateTime
  exception DuplicateKey of path
  fun digitToInt c = Char.ord c - Char.ord #"0"
end
signature PARSE_TOML = sig
  type value = TomlValue.value
  type table = (string * TomlValue.value) list
  type path = string list
  exception UnexpectedEndOfInput
  exception UnknownEscapeChar
  exception InvalidString
  exception UnexpectedChar
  exception InvalidNumber
  exception InvalidDateTime
  exception DuplicateKey of path
  val parse : (char, 'strm) StringCvt.reader -> 'strm -> table
end
structure ParseToml :> PARSE_TOML = struct
  open Common
  val implodeRev = StringExt.implodeRev
  type value = TomlValue.value
  type table = (string * value) list
  (*: val parse : (char, 'strm) StringCvt.reader -> 'strm -> table *)
  fun parse (getc : (char, 'strm) StringCvt.reader)
    = let (*: val expect : char * 'strm -> 'strm *)
          fun expect (c, strm) = case getc strm of
                                     NONE => raise UnexpectedEndOfInput
                                   | SOME (c', strm') => if c = c' then
                                                             strm'
                                                         else
                                                             raise UnexpectedChar
          (*: val skipWhiteSpace : 'strm -> 'strm *)
          fun skipWhiteSpace strm = case getc strm of
                                        NONE => strm
                                      | SOME (c, strm') => if c = #" " orelse c = #"\t" then
                                                               skipWhiteSpace strm'
                                                           else
                                                               strm
          (*: val skipWhiteSpaceAndGetc : 'strm -> (char * 'strm) option *)
          (* skipWhiteSpaceAndGetc strm = getc (skipWhiteSpace strm) *)
          fun skipWhiteSpaceAndGetc strm = case getc strm of
                                               SOME (#" ", strm') => skipWhiteSpaceAndGetc strm'
                                             | SOME (#"\t", strm') => skipWhiteSpaceAndGetc strm'
                                             | r => r
          (*: val skipUntilNewline : 'strm -> 'strm *)
          fun skipUntilNewline strm = case getc strm of
                                          NONE => strm
                                        | SOME (#"\n", strm') => strm'
                                        | SOME (#"\r", strm') => expect (#"\n", strm')
                                        | SOME (c, strm') => if Char.isCntrl c then
                                                                 raise UnexpectedChar
                                                             else
                                                                 skipUntilNewline strm'
          (*: val skipWhiteSpaceOrComment : 'strm -> 'strm *)
          fun skipWhiteSpaceOrComment strm = case getc strm of
                                                 NONE => strm
                                               | SOME (#"#", strm') => skipUntilNewline strm'
                                               | SOME (#" ", strm') => skipWhiteSpaceOrComment strm'
                                               | SOME (#"\t", strm') => skipWhiteSpaceOrComment strm'
                                               | SOME (#"\r", strm') => expect (#"\n", strm')
                                               | SOME (#"\n", strm') => strm'
                                               | SOME (_, _) => raise UnexpectedChar
          (*: val skipWhiteSpaceOrCommentOrNewlineAndGetc : 'strm -> (char * 'strm) option *)
          fun skipWhiteSpaceOrCommentOrNewlineAndGetc strm = case getc strm of
                                                                 SOME (#"#", strm') => skipWhiteSpaceOrCommentOrNewlineAndGetc (skipUntilNewline strm')
                                                               | SOME (#" ", strm') => skipWhiteSpaceOrCommentOrNewlineAndGetc strm'
                                                               | SOME (#"\t", strm') => skipWhiteSpaceOrCommentOrNewlineAndGetc strm'
                                                               | SOME (#"\r", strm') => skipWhiteSpaceOrCommentOrNewlineAndGetc (expect (#"\n", strm'))
                                                               | SOME (#"\n", strm') => skipWhiteSpaceOrCommentOrNewlineAndGetc strm'
                                                               | r => r
          (*: val skipOptionalNewline : 'strm -> 'strm *)
          fun skipOptionalNewline strm = case getc strm of
                                             SOME (#"\n", strm') => strm'
                                           | SOME (#"\r", strm') => expect (#"\n", strm')
                                           | _ => strm
          (*: val skipWhiteSpaceOrNewline : 'strm -> 'strm *)
          fun skipWhiteSpaceOrNewline strm = case getc strm of
                                                 NONE => strm
                                               | SOME (#"\r", strm') => skipWhiteSpaceOrNewline (expect (#"\n", strm'))
                                               | SOME (c, strm') => if c = #" " orelse c = #"\t" orelse c = #"\n" then
                                                                        skipWhiteSpaceOrNewline strm'
                                                                    else
                                                                        strm
          (*: val readHexDigit : 'strm -> int * 'strm *)
          fun readHexDigit strm
            = case getc strm of
                  NONE => raise UnexpectedEndOfInput
                | SOME (c, strm') => if #"0" <= c andalso c <= #"9" then
                                       (digitToInt c, strm')
                                     else if #"A" <= c andalso c <= #"F" then
                                       (Char.ord c - (Char.ord #"A" - 10), strm')
                                     else if #"a" <= c andalso c <= #"f" then
                                       (Char.ord c - (Char.ord #"a" - 10), strm')
                                     else
                                       raise UnknownEscapeChar
          (*: val readFourHexDigit : 'strm -> int * 'strm *)
          fun readFourHexDigit strm
            = let val (c3, strm) = readHexDigit strm
                  val (c2, strm) = readHexDigit strm
                  val (c1, strm) = readHexDigit strm
                  val (c0, strm) = readHexDigit strm
              in (((c3 * 16 + c2) * 16 + c1) * 16 + c0, strm)
              end
          (*: val revAppendUtf8 : word * char list -> char list *)
          fun revAppendUtf8 (i, accum) = if i < 0wx80 then (* 7-bit *)
                                             Char.chr (Word.toInt i) :: accum
                                         else if i < 0wx800 then (* 11-bit *)
                                             let val u0 = Word.orb (0wxC0, Word.>> (i, 0w6))
                                                 val u1 = Word.orb (0wx80, Word.andb (i, 0wx3F))
                                             in Char.chr (Word.toInt u1) :: Char.chr (Word.toInt u0) :: accum
                                             end
                                         else if i < 0wx10000 then (* 16-bit *)
                                             if 0wxD800 <= i andalso i < 0wxE000 then
                                                 raise UnknownEscapeChar (* invalid Unicode scalar *)
                                             else
                                                 let val u0 = Word.orb (0wxE0, Word.>> (i, 0w12))
                                                     val u1 = Word.orb (0wx80, Word.andb (Word.>> (i, 0w6), 0wx3F))
                                                     val u2 = Word.orb (0wx80, Word.andb (i, 0wx3F))
                                                 in Char.chr (Word.toInt u2) :: Char.chr (Word.toInt u1) :: Char.chr (Word.toInt u0) :: accum
                                                 end
                                         else if i < 0wx110000 then (* 21-bit *)
                                             let val u0 = Word.orb (0wxF0, Word.>> (i, 0w18))
                                                 val u1 = Word.orb (0wx80, Word.andb (Word.>> (i, 0w12), 0wx3F))
                                                 val u2 = Word.orb (0wx80, Word.andb (Word.>> (i, 0w6), 0wx3F))
                                                 val u3 = Word.orb (0wx80, Word.andb (i, 0wx3F))
                                             in Char.chr (Word.toInt u3) :: Char.chr (Word.toInt u2) :: Char.chr (Word.toInt u1) :: Char.chr (Word.toInt u0) :: accum
                                             end
                                         else
                                             raise UnknownEscapeChar (* invalid Unicode scalar *)
          local
            fun go (accum, strm) = case getc strm of
                                       NONE => raise UnexpectedEndOfInput
                                     | SOME (#"\"", strm') => (implodeRev accum, strm')
                                     | SOME (#"\\", strm') => (case getc strm' of
                                                                   NONE => raise UnexpectedEndOfInput
                                                                 | SOME (#"\"", strm'') => go (#"\"" :: accum, strm'')
                                                                 | SOME (#"\\", strm'') => go (#"\\" :: accum, strm'')
                                                                 | SOME (#"b", strm'') => go (#"\b" :: accum, strm'')
                                                                 | SOME (#"f", strm'') => go (#"\f" :: accum, strm'')
                                                                 | SOME (#"n", strm'') => go (#"\n" :: accum, strm'')
                                                                 | SOME (#"r", strm'') => go (#"\r" :: accum, strm'')
                                                                 | SOME (#"t", strm'') => go (#"\t" :: accum, strm'')
                                                                 | SOME (#"u", strm'') => let val (i, strm''') = readFourHexDigit strm''
                                                                                          in go (revAppendUtf8 (Word.fromInt i, accum), strm''')
                                                                                          end
                                                                 | SOME (#"U", strm'') => let val (hi, strm''') = readFourHexDigit strm''
                                                                                              val (lo, strm'''') = readFourHexDigit strm'''
                                                                                          in go (revAppendUtf8 (Word.fromInt (hi * 65536 + lo), accum), strm'''')
                                                                                          end
                                                                 | SOME (_, _) => raise UnknownEscapeChar
                                                              )
                                     | SOME (c, strm') => if (c < #" " andalso c <> #"\t") orelse c = #"\u007f" then
                                                              raise InvalidString
                                                          else
                                                              go (c :: accum, strm')
          in
            (*: val readBasicString : 'strm -> string * 'strm *)
            fun readBasicString strm = go ([], strm)
          end
          fun checkAllowedChar c = if (c < #" " andalso c <> #"\t" andalso c <> #"\n") orelse c = #"\u007f" then
                                       raise InvalidString
                                   else
                                       c
          local
            fun go (accum, strm) = case getc strm of
                                       NONE => raise UnexpectedEndOfInput
                                     | SOME (#"\\", strm') => escape (accum, strm')
                                     | SOME (#"\"", strm') =>
                                       (case getc strm' of
                                            SOME (#"\"", strm'') =>
                                            (case getc strm'' of
                                                 SOME (#"\"", strm''') =>
                                                 (case getc strm''' of
                                                     SOME (#"\"", strm'''') =>
                                                     (case getc strm'''' of
                                                          SOME (#"\"", strm''''') => (implodeRev (#"\"" :: #"\"" :: accum), strm''''')
                                                        | _ => (implodeRev (#"\"" :: accum), strm'''')
                                                     )
                                                   | _ => (implodeRev accum, strm''')
                                                 )
                                               | SOME (#"\\", strm''') => escape (#"\"" :: #"\"" :: accum, strm''')
                                               | SOME (#"\r", strm''') => go (#"\n" :: #"\r" :: #"\"" :: #"\"" :: accum, expect (#"\n", strm'''))
                                               | SOME (c, strm''') => go (checkAllowedChar c :: #"\"" :: #"\"" :: accum, strm''')
                                               | NONE => raise UnexpectedEndOfInput
                                            )
                                          | SOME (#"\\", strm'') => escape (#"\"" :: accum, strm'')
                                          | SOME (#"\r", strm'') => go (#"\n" :: #"\r" :: #"\"" :: accum, expect (#"\n", strm''))
                                          | SOME (c, strm''') => go (checkAllowedChar c :: #"\"" :: accum, strm''')
                                          | NONE => raise UnexpectedEndOfInput
                                        )
                                     | SOME (#"\r", strm') => go (#"\n" :: #"\r" :: accum, expect (#"\n", strm'))
                                     | SOME (c, strm') => go (checkAllowedChar c :: accum, strm')
            and escape (accum, strm) = case getc strm of
                                           NONE => raise UnexpectedEndOfInput
                                         | SOME (#"\"", strm') => go (#"\"" :: accum, strm')
                                         | SOME (#"\\", strm') => go (#"\\" :: accum, strm')
                                         | SOME (#"b", strm') => go (#"\b" :: accum, strm')
                                         | SOME (#"f", strm') => go (#"\f" :: accum, strm')
                                         | SOME (#"n", strm') => go (#"\n" :: accum, strm')
                                         | SOME (#"r", strm') => go (#"\r" :: accum, strm')
                                         | SOME (#"t", strm') => go (#"\t" :: accum, strm')
                                         | SOME (#"u", strm') => let val (i, strm'') = readFourHexDigit strm'
                                                                 in go (revAppendUtf8 (Word.fromInt i, accum), strm'')
                                                                 end
                                         | SOME (#"U", strm') => let val (hi, strm'') = readFourHexDigit strm'
                                                                     val (lo, strm''') = readFourHexDigit strm''
                                                                 in go (revAppendUtf8 (Word.fromInt (hi * 65536 + lo), accum), strm''')
                                                                 end
                                         | SOME (c, strm') => if c = #" " orelse c = #"\t" then
                                                                  case skipWhiteSpaceAndGetc strm' of
                                                                      NONE => raise UnexpectedEndOfInput
                                                                    | SOME (#"\n", strm'') => go (accum, skipWhiteSpaceOrNewline strm'')
                                                                    | SOME (#"\r", strm'') => go (accum, skipWhiteSpaceOrNewline (expect (#"\n", strm'')))
                                                                    | SOME (_, _) => raise UnknownEscapeChar (* expected a newline *)
                                                              else if c = #"\r" then
                                                                  go (accum, skipWhiteSpaceOrNewline (expect (#"\n", strm')))
                                                              else if c = #"\n" then
                                                                  go (accum, skipWhiteSpaceOrNewline strm')
                                                              else
                                                                  raise UnknownEscapeChar
          in
            (*: val readMultilineBasicString : 'strm -> string * 'strm *)
            fun readMultilineBasicString strm = go ([], strm)
          end
          local
            fun go (accum, strm) = case getc strm of
                                       NONE => raise UnexpectedEndOfInput
                                     | SOME (#"'", strm') => (implodeRev accum, strm')
                                     | SOME (c, strm') => if (c < #" " andalso c <> #"\t") orelse c = #"\u007f" then
                                                              raise InvalidString
                                                          else
                                                              go (c :: accum, strm')
          in
            (*: val readLiteralString : 'strm -> string * 'strm *)
            fun readLiteralString strm = go ([], strm)
          end
          local
            fun go (accum, strm) = case getc strm of
                                       NONE => raise UnexpectedEndOfInput
                                     | SOME (#"'", strm') =>
                                       (case getc strm' of
                                            SOME (#"'", strm'') =>
                                            (case getc strm'' of
                                                 SOME (#"'", strm''') => 
                                                 (case getc strm''' of
                                                     SOME (#"'", strm'''') =>
                                                     (case getc strm'''' of
                                                          SOME (#"'", strm''''') => (implodeRev (#"'" :: #"'" :: accum), strm''''')
                                                        | _ => (implodeRev (#"'" :: accum), strm'''')
                                                     )
                                                   | _ => (implodeRev accum, strm''')
                                                 )
                                               | SOME (c, strm''') => go (checkAllowedChar c :: #"'" :: #"'" :: accum, strm''')
                                               | NONE => raise UnexpectedEndOfInput
                                            )
                                          | SOME (c, strm'') => go (checkAllowedChar c :: #"'" :: accum, strm'')
                                          | NONE => raise UnexpectedEndOfInput
                                       )
                                     | SOME (c, strm') => go (checkAllowedChar c :: accum, strm')
          in
            (*: val readMultilineLiteralString : 'strm -> string * 'strm *)
            fun readMultilineLiteralString strm = go ([], strm)
          end
          (*: val readHexInt : IntInf.int * 'strm -> IntInf.int * 'strm *)
          fun readHexInt (accum : IntInf.int, strm) = case getc strm of
                                                          NONE => raise UnexpectedEndOfInput
                                                        | SOME (c, strm') => if #"0" <= c andalso c <= #"9" then
                                                                                 readHexIntUnderscore (accum * 16 + IntInf.fromInt (digitToInt c), strm')
                                                                             else if #"A" <= c andalso c <= #"F" then
                                                                                 readHexIntUnderscore (accum * 16 + IntInf.fromInt (Char.ord c - (Char.ord #"A" - 10)), strm')
                                                                             else if #"a" <= c andalso c <= #"f" then
                                                                                 readHexIntUnderscore (accum * 16 + IntInf.fromInt (Char.ord c - (Char.ord #"a" - 10)), strm')
                                                                             else
                                                                                 raise UnexpectedChar
          and readHexIntUnderscore (accum, strm) = case getc strm of
                                                       NONE => (accum, strm)
                                                     | SOME (#"_", strm') => readHexInt (accum, strm') (* the next character must be a hex digit *)
                                                     | SOME (c, strm') => if #"0" <= c andalso c <= #"9" then
                                                                              readHexIntUnderscore (accum * 16 + IntInf.fromInt (digitToInt c), strm')
                                                                          else if #"A" <= c andalso c <= #"F" then
                                                                              readHexIntUnderscore (accum * 16 + IntInf.fromInt (Char.ord c - (Char.ord #"A" - 10)), strm')
                                                                          else if #"a" <= c andalso c <= #"f" then
                                                                              readHexIntUnderscore (accum * 16 + IntInf.fromInt (Char.ord c - (Char.ord #"a" - 10)), strm')
                                                                          else
                                                                              (accum, strm)
          (*: val readOctInt : IntInf.int * 'strm -> IntInf.int * 'strm *)
          fun readOctInt (accum : IntInf.int, strm) = case getc strm of
                                                          NONE => raise UnexpectedEndOfInput
                                                        | SOME (c, strm') => if #"0" <= c andalso c <= #"7" then
                                                                                 readOctIntUnderscore (accum * 8 + IntInf.fromInt (digitToInt c), strm')
                                                                             else
                                                                                 raise UnexpectedChar
          and readOctIntUnderscore (accum, strm) = case getc strm of
                                                       NONE => (accum, strm)
                                                     | SOME (#"_", strm') => readOctInt (accum, strm') (* the next character must be a oct digit *)
                                                     | SOME (c, strm') => if #"0" <= c andalso c <= #"7" then
                                                                              readOctIntUnderscore (accum * 8 + IntInf.fromInt (digitToInt c), strm')
                                                                          else
                                                                              (accum, strm)
          (*: val readBinInt : IntInf.int * 'strm -> IntInf.int * 'strm *)
          fun readBinInt (accum : IntInf.int, strm) = case getc strm of
                                                          NONE => raise UnexpectedEndOfInput
                                                        | SOME (#"0", strm') => readBinIntUnderscore (accum * 2, strm')
                                                        | SOME (#"1", strm') => readBinIntUnderscore (accum * 2 + 1, strm')
                                                        | SOME (_, _) => raise UnexpectedChar
          and readBinIntUnderscore (accum, strm) = case getc strm of
                                                       NONE => (accum, strm)
                                                     | SOME (#"_", strm') => readBinInt (accum, strm') (* the next character must be a bin digit *)
                                                     | SOME (#"0", strm') => readBinIntUnderscore (accum * 2, strm')
                                                     | SOME (#"1", strm') => readBinIntUnderscore (accum * 2 + 1, strm')
                                                     | SOME (_, _) => (accum, strm)
          (*: val readDecInt : char list * bool * 'strm -> char list * bool * 'strm *)
          fun readDecInt (accum, hadUnderscore, strm) = case getc strm of
                                                            NONE => raise UnexpectedEndOfInput
                                                          | SOME (c, strm') => if Char.isDigit c then
                                                                                   readDecIntUnderscore (c :: accum, hadUnderscore, strm')
                                                                               else
                                                                                   raise UnexpectedChar
          and readDecIntUnderscore (accum, hadUnderscore, strm) = case getc strm of
                                                                      NONE => (accum, hadUnderscore, strm)
                                                                    | SOME (#"_", strm') => readDecInt (accum, true, strm')
                                                                    | SOME (c, strm') => if Char.isDigit c then
                                                                                             readDecIntUnderscore (c :: accum, hadUnderscore, strm')
                                                                                         else
                                                                                             (accum, hadUnderscore, strm)
          (*: val readExpPart : char list * char * 'strm -> value * 'strm *)
          fun readExpPart (accum', e, strm'') = let val (accum'', _, strm''') = case getc strm'' of
                                                                                    NONE => raise UnexpectedEndOfInput
                                                                                  | SOME (#"+", strm''') => readDecInt (#"+" :: e :: accum', false, strm''')
                                                                                  | SOME (#"-", strm''') => readDecInt (#"-" :: e :: accum', false, strm''')
                                                                                  | SOME (c, strm''') => if Char.isDigit c then
                                                                                                             readDecIntUnderscore (c :: e :: accum', false, strm''')
                                                                                                         else
                                                                                                             raise UnexpectedChar
                                                in (TomlValue.FLOAT (implodeRev accum''), strm''')
                                                end
          (*: val readSigned : char list * 'strm -> value * 'strm *) (* dec-int / float *)
          fun readSigned (sign, d0, strm) = let val (accum, _, strm') = readDecIntUnderscore ([d0, sign], false, strm)
                                                fun checkPrefixZero () = case accum of
                                                                            [_, _] => ()
                                                                          | _ => if d0 = #"0" then
                                                                                      raise InvalidNumber
                                                                                  else
                                                                                      ()

                                            in case getc strm' of
                                                   SOME (#".", strm'') => let val () = checkPrefixZero () (* float *)
                                                                              val (accum', _, strm''') = readDecInt (#"." :: accum, false, strm'')
                                                                          in case getc strm''' of
                                                                                 SOME (#"e", strm'''') => readExpPart (accum', #"e", strm'''')
                                                                               | SOME (#"E", strm'''') => readExpPart (accum', #"E", strm'''')
                                                                               | _ => (TomlValue.FLOAT (implodeRev accum'), strm''')
                                                                          end
                                                 | SOME (#"e", strm'') => (checkPrefixZero (); readExpPart (accum, #"e", strm'')) (* float *)
                                                 | SOME (#"E", strm'') => (checkPrefixZero (); readExpPart (accum, #"E", strm'')) (* float *)
                                                 | _ => (checkPrefixZero (); (TomlValue.INTEGER (Option.valOf (IntInf.fromString (implodeRev accum))), strm')) (* dec-int *)
                                            end
          fun readTwoDigit (accum, strm) = case getc strm of
                                               NONE => raise UnexpectedEndOfInput
                                             | SOME (c0, strm') => if Char.isDigit c0 then
                                                                       case getc strm' of
                                                                           NONE => raise UnexpectedEndOfInput
                                                                         | SOME (c1, strm'') => if Char.isDigit c1 then
                                                                                                    (c1 :: c0 :: accum, digitToInt c0 * 10 + digitToInt c1, strm'')
                                                                                                else
                                                                                                    raise UnexpectedChar
                                                                   else
                                                                       raise UnexpectedChar
          local
            fun readDigits (accum, strm) = case getc strm of
                                               NONE => (accum, strm)
                                             | SOME (c, strm') => if Char.isDigit c then
                                                                      readDigits (c :: accum, strm')
                                                                  else
                                                                      (accum, strm)
          in
            (*: val readMinSec : char list * 'strm -> char list * 'strm *)
            fun readMinSec (accum, strm) = let val (accum', min, strm') = readTwoDigit (accum, strm) (* minute *)
                                               val strm'' = expect (#":", strm')
                                               val (accum'', sec, strm''') = readTwoDigit (#":" :: accum', strm'') (* second *)
                                           in if min < 60 andalso sec <= 60 then
                                                  ()
                                              else
                                                  raise InvalidDateTime
                                            ; case getc strm''' of
                                                  SOME (#".", strm'''') => readDigits (#"." :: accum'', strm'''') (* secfrac *)
                                                | _ => (accum'', strm''')
                                           end
          end
          fun readTimePart (accum, strm) = let val (accum', hour, strm') = readTwoDigit (accum, strm)
                                               val () = if hour < 24 then
                                                            ()
                                                        else
                                                            raise InvalidDateTime
                                               val (accum'', strm'') = readMinSec (#":" :: accum', expect (#":", strm'))
                                           in case getc strm'' of
                                                  SOME (#"Z", strm''') => (TomlValue.DATETIME (implodeRev (#"Z" :: accum'')), strm''') (* offset-date-time *)
                                                | SOME (#"z", strm''') => (TomlValue.DATETIME (implodeRev (#"z" :: accum'')), strm''') (* offset-date-time *)
                                                | SOME (#"+", strm''') => let val (accum''', offsetHour, strm'''') = readTwoDigit (#"+" :: accum'', strm''') (* offset-date-time *)
                                                                              val strm''''' = expect (#":", strm'''')
                                                                              val (accum'''', offsetMin, strm'''''') = readTwoDigit (#":" :: accum''', strm''''')
                                                                              val () = if offsetHour < 24 andalso offsetMin < 60 then
                                                                                           ()
                                                                                       else
                                                                                           raise InvalidDateTime
                                                                          in (TomlValue.DATETIME (implodeRev accum''''), strm'''''')
                                                                          end
                                                | SOME (#"-", strm''') => let val (accum''', offsetHour, strm'''') = readTwoDigit (#"-" :: accum'', strm''') (* offset-date-time *)
                                                                              val strm''''' = expect (#":", strm'''')
                                                                              val (accum'''', offsetMin, strm'''''') = readTwoDigit (#":" :: accum''', strm''''')
                                                                              val () = if offsetHour < 24 andalso offsetMin < 60 then
                                                                                           ()
                                                                                       else
                                                                                           raise InvalidDateTime
                                                                          in (TomlValue.DATETIME (implodeRev accum''''), strm'''''')
                                                                          end
                                                | _ => (TomlValue.LOCAL_DATETIME (implodeRev accum''), strm'') (* local-date-time *)
                                           end
          fun isValidDate (_, 1, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (year, 2, 29) = Int.rem (year, 4) = 0 andalso (Int.rem (year, 100) <> 0 orelse Int.rem (year, 400) = 0)
            | isValidDate (_, 2, mday) = 1 <= mday andalso mday <= 28
            | isValidDate (_, 3, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, 4, mday) = 1 <= mday andalso mday <= 30
            | isValidDate (_, 5, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, 6, mday) = 1 <= mday andalso mday <= 30
            | isValidDate (_, 7, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, 8, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, 9, mday) = 1 <= mday andalso mday <= 30
            | isValidDate (_, 10, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, 11, mday) = 1 <= mday andalso mday <= 30
            | isValidDate (_, 12, mday) = 1 <= mday andalso mday <= 31
            | isValidDate (_, _, _) = false
          (*: val readDate : char list * int * 'strm -> value * 'strm *) (* full-date: offset-date-time / local-date-time / local-date *)
          fun readDate (accum, year, strm) = let val (accum', month, strm') = readTwoDigit (accum, strm) (* month *)
                                                 val strm'' = expect (#"-", strm')
                                                 val (accum'', mday, strm''') = readTwoDigit (accum', strm'') (* mday; range is not checked *)
                                                 val () = if isValidDate (year, month, mday) then
                                                              ()
                                                          else
                                                              raise InvalidDateTime
                                             in case getc strm''' of
                                                    SOME (#"T", strm'''') => readTimePart (#"T" :: accum'', strm'''') (* offset-date-time / local-date-time *)
                                                  | SOME (#"t", strm'''') => readTimePart (#"T" :: accum'', strm'''') (* offset-date-time / local-date-time *)
                                                  | SOME (#" ", strm'''') => (case getc strm'''' of
                                                                                  SOME (c, _) => if Char.isDigit c then
                                                                                                     readTimePart (#"T" :: accum'', strm'''') (* offset-date-time / local-date-time *)
                                                                                                 else
                                                                                                     (TomlValue.DATE (implodeRev accum''), strm''') (* local-date *)
                                                                                | _ => (TomlValue.DATE (implodeRev accum''), strm''') (* local-date *)
                                                                             )
                                                  | _ => (TomlValue.DATE (implodeRev accum''), strm''') (* local-date *)
                                             end
          (*: val readUnsigned : char * 'strm -> value * 'strm *) (* dec-int / float / date-time / date / time *)
          fun readUnsigned (d0, strm) = let val (revDigits, hadUnderscore, strm') = readDecIntUnderscore ([d0], false, strm)
                                            fun checkPrefixZero () = case revDigits of
                                                                         [_] => ()
                                                                       | _ => if d0 = #"0" then
                                                                                  raise InvalidNumber
                                                                              else
                                                                                  ()
                                        in case (revDigits, hadUnderscore, getc strm') of
                                               ([d3, d2, d1, _], false, SOME (#"-", strm'')) => (* full-date: offset-date-time / local-date-time / local-date *)
                                               let val year = digitToInt d0 * 10 + digitToInt d1 * 10 + digitToInt d2 + digitToInt d3
                                               in readDate (#"-" :: revDigits, year, strm'')
                                               end
                                             | ([_, _], false, SOME (#":", strm'')) => let val (accum, strm''') = readMinSec (#":" :: revDigits, strm'') (* partial-time *)
                                                                                       in (TomlValue.TIME (implodeRev accum), strm''')
                                                                                       end
                                             | (_, _, SOME (#".", strm'')) => let val () = checkPrefixZero () (* float *)
                                                                                  val (accum, _, strm''') = readDecInt (#"." :: revDigits, false, strm'')
                                                                              in case getc strm''' of
                                                                                     SOME (#"e", strm'''') => readExpPart (accum, #"e", strm'''')
                                                                                   | SOME (#"E", strm'''') => readExpPart (accum, #"E", strm'''')
                                                                                   | _ => (TomlValue.FLOAT (implodeRev accum), strm''')
                                                                              end
                                             | (_, _, SOME (#"e", strm'')) => (checkPrefixZero (); readExpPart (revDigits, #"e", strm'')) (* float *)
                                             | (_, _, SOME (#"E", strm'')) => (checkPrefixZero (); readExpPart (revDigits, #"E", strm'')) (* float *)
                                             | (_, _, _) => (checkPrefixZero (); (TomlValue.INTEGER (Option.valOf (IntInf.fromString (implodeRev revDigits))), strm')) (* dec-int *)
                                        end
          fun isValidUnquotedKey c = Char.isAlphaNum c orelse c = #"-" orelse c = #"_"
          (*: val readSimpleKey : (char * 'strm) option -> string * 'strm *)
          fun readSimpleKey NONE = raise UnexpectedEndOfInput (* expected a key *)
            | readSimpleKey (SOME (#"\"", strm)) = readBasicString strm
            | readSimpleKey (SOME (#"'", strm)) = readLiteralString strm
            | readSimpleKey (SOME (c0, strm))
                = let fun go (accum, strm') = case getc strm' of
                                                  NONE => (implodeRev accum, strm')
                                                | SOME (c, strm'') => if isValidUnquotedKey c then
                                                                          go (c :: accum, strm'')
                                                                      else
                                                                          (implodeRev accum, strm')
                  in if isValidUnquotedKey c0 then
                         go ([c0], strm)
                     else
                         raise UnexpectedChar
                  end
          (*: val readKey : (char * 'strm) option -> key * 'strm *)
          fun readKey input = let fun go (accum, strm) = case getc strm of
                                                             SOME (#".", strm') => let val (k, strm'') = readSimpleKey (skipWhiteSpaceAndGetc strm')
                                                                                   in go (k :: accum, skipWhiteSpace strm'')
                                                                                   end
                                                           | _ => (List.rev accum, strm)
                                  val (k0, strm) = readSimpleKey input
                              in go ([k0], skipWhiteSpace strm)
                              end
          datatype defined_by = EXACT_HEADER | IMPLICIT_HEADER | IMPLICIT_KEYVAL
          datatype partial_value = LEAF of value
                                 | PARTIAL_TABLE of defined_by * partial_table
                                 | PARTIAL_ARRAY of partial_table * partial_table list (* in reverse order *)
          withtype partial_table = (string * partial_value) list
          (*:
          val finalize : partial_table -> value
          val finalizeValue : partial_value -> value
          val finalizeTable : partial_table -> table
          *)
          fun finalize xs = TomlValue.TABLE (finalizeTable xs)
          and finalizeValue (LEAF v) = v
            | finalizeValue (PARTIAL_TABLE (_, pt)) = finalize pt
            | finalizeValue (PARTIAL_ARRAY (last, xs)) = TomlValue.ARRAY (List.revAppend (List.map finalize xs, [finalize last]))
          and finalizeTable xs = List.map (fn (key, pv) => (key, finalizeValue pv)) xs
          (*: val insert : path * partial_table * key * value -> partial_table *)
          fun insert (revPath, pt, keys as [key], v) = if List.exists (fn (key', _) => key = key') pt then
                                                           raise DuplicateKey (List.revAppend (revPath, keys))
                                                       else
                                                           pt @ [(key, LEAF v)]
            | insert (revPath, pt, key :: keys, v)
                = let fun go (accum, []) = List.revAppend (accum, [(key, PARTIAL_TABLE (IMPLICIT_KEYVAL, insert (key :: revPath, [], keys, v)))]) (* new key *)
                        | go (accum, (p as (key', pv)) :: xs)
                            = if key = key' then
                                  case pv of
                                      LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                    | PARTIAL_TABLE (EXACT_HEADER, _) => raise DuplicateKey (List.revAppend (revPath, [key]))
                                    | PARTIAL_TABLE (definedBy, pt') =>
                                      List.revAppend (accum, (key', PARTIAL_TABLE (definedBy, insert (key :: revPath, pt', keys, v))) :: xs)
                                    | PARTIAL_ARRAY _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                              else
                                  go (p :: accum, xs)
                  in go ([], pt)
                  end
            | insert (_, _, [], _) = raise Match (* should not occur *)
          (*:
          val readValue : path * (char * 'strm) option -> value * 'strm
          val readArray : value list * 'strm -> value * 'strm
          val readInlineTable : value list * 'strm -> value * 'strm
          val readKeyval : (char * 'strm) option -> string list * value * 'strm
          *)
          fun readValue (_, NONE) = raise UnexpectedEndOfInput
            | readValue (revPath, SOME (c, strm))
                = case c of
                      #"\"" => (case getc strm of (* ml-basic-string / basic-string *)
                                    NONE => raise UnexpectedEndOfInput
                                  | SOME (#"\"", strm') =>
                                    (case getc strm' of
                                         SOME (#"\"", strm'') => let val (s, strm''') = readMultilineBasicString (skipOptionalNewline strm'')
                                                                 in (TomlValue.STRING s, strm''')
                                                                 end
                                       | _ => (TomlValue.STRING "", strm')
                                    )
                                  | _ => let val (s, strm') = readBasicString strm
                                         in (TomlValue.STRING s, strm')
                                         end
                               )
                    | #"'" => (case getc strm of (* ml-literal-string / literal-string *)
                                   NONE => raise UnexpectedEndOfInput
                                 | SOME (#"'", strm') =>
                                   (case getc strm' of
                                        SOME (#"'", strm'') => let val (s, strm''') = readMultilineLiteralString (skipOptionalNewline strm'')
                                                               in (TomlValue.STRING s, strm''')
                                                               end
                                      | _ => (TomlValue.STRING "", strm')
                                   )
                                 | _ => let val (s, strm') = readLiteralString strm
                                        in (TomlValue.STRING s, strm')
                                        end
                              )
                    | #"[" => readArray ("[]" :: revPath, [], strm) (* array *)
                    | #"{" => (case skipWhiteSpaceAndGetc strm of (* inline-table *)
                                   SOME (#"}", strm') => (TomlValue.TABLE [], strm')
                                 | r => readInlineTable (revPath, [], r)
                              )
                    | #"t" => (TomlValue.BOOLEAN true, expect (#"e", expect (#"u", expect (#"r", strm))))
                    | #"f" => (TomlValue.BOOLEAN false, expect (#"e", expect (#"s", expect (#"l", expect (#"a", strm)))))
                    | #"i" => (TomlValue.FLOAT "inf", expect (#"f", expect (#"n", strm)))
                    | #"n" => (TomlValue.FLOAT "nan", expect (#"n", expect (#"a", strm)))
                    | #"+" => (case getc strm of
                                   NONE => raise UnexpectedEndOfInput
                                 | SOME (#"i", strm') => (TomlValue.FLOAT "+inf", expect (#"f", expect (#"n", strm')))
                                 | SOME (#"n", strm') => (TomlValue.FLOAT "+nan", expect (#"n", expect (#"a", strm')))
                                 | SOME (c', strm') => if Char.isDigit c' then
                                                           readSigned (#"+", c', strm') (* float / integer *)
                                                       else
                                                           raise UnexpectedChar
                              )
                    | #"-" => (case getc strm of
                                   NONE => raise UnexpectedEndOfInput
                                 | SOME (#"i", strm') => (TomlValue.FLOAT "-inf", expect (#"f", expect (#"n", strm')))
                                 | SOME (#"n", strm') => (TomlValue.FLOAT "-nan", expect (#"n", expect (#"a", strm')))
                                 | SOME (c', strm') => if Char.isDigit c' then
                                                           readSigned (#"-", c', strm') (* float / integer *)
                                                       else
                                                           raise UnexpectedChar
                              )
                    | #"0" => (case getc strm of
                                   NONE => (TomlValue.INTEGER 0, strm)
                                 | SOME (#"x", strm') => let val (x, strm'') = readHexInt (0, strm') (* hex-int *)
                                                         in (TomlValue.INTEGER x, strm'')
                                                         end
                                 | SOME (#"o", strm') => let val (x, strm'') = readOctInt (0, strm') (* oct-int *)
                                                         in (TomlValue.INTEGER x, strm'')
                                                         end
                                 | SOME (#"b", strm') => let val (x, strm'') = readBinInt (0, strm') (* bin-int *)
                                                         in (TomlValue.INTEGER x, strm'')
                                                         end
                                 | SOME (_, _) => readUnsigned (#"0", strm) (* date-time / float / unsigned-dec-int *)
                              )
                    | _ => if Char.isDigit c then
                               readUnsigned (c, strm) (* date-time / float / integer *)
                           else
                               raise UnexpectedChar
          and readArray (revPath, accum, strm) = (case skipWhiteSpaceOrCommentOrNewlineAndGetc strm of
                                                      SOME (#"]", strm') => (TomlValue.ARRAY (List.rev accum), strm')
                                                    | r => let val (v, strm') = readValue (revPath, r)
                                                           in case skipWhiteSpaceOrCommentOrNewlineAndGetc strm' of
                                                                  NONE => raise UnexpectedEndOfInput
                                                                | SOME (#",", strm'') => readArray (revPath, v :: accum, strm'')
                                                                | SOME (#"]", strm'') => (TomlValue.ARRAY (List.rev (v :: accum)), strm'')
                                                                | SOME (_, _) => raise UnexpectedChar
                                                           end
                                                 )
          and readInlineTable (revPath, accum, r) = let val (key, v, strm) = readKeyval (revPath, r)
                                                        val accum' = insert (revPath, accum, key, v)
                                                    in case skipWhiteSpaceAndGetc strm of
                                                           NONE => raise UnexpectedEndOfInput
                                                         | SOME (#",", strm') => readInlineTable (revPath, accum', skipWhiteSpaceAndGetc strm')
                                                         | SOME (#"}", strm') => (finalize accum', strm')
                                                         | SOME (_, _) => raise UnexpectedChar
                                                    end
          and readKeyval (revPath, r) = let val (key, strm') = readKey r
                                            val (v, strm'') = readValue (List.revAppend (key, revPath), skipWhiteSpaceAndGetc (expect (#"=", strm')))
                                        in (key, v, strm'')
                                        end
          datatype expression = KEYVAL of key * TomlValue.value
                              | STD_TABLE of key
                              | ARRAY_TABLE of key
          (*: val readExpression : string list * 'strm -> (expression * 'strm) option *)
          fun readExpression (revPath, strm)
            = case skipWhiteSpaceAndGetc strm of
                  NONE => NONE
                | SOME (#"\n", strm') => readExpression (revPath, strm')
                | SOME (#"\r", strm') => readExpression (revPath, expect (#"\n", strm'))
                | SOME (#"#", strm') => readExpression (revPath, skipUntilNewline strm')
                | SOME (#"[", strm') => (* table *)
                  (case getc strm' of
                       SOME (#"[", strm'') => let val (key, strm''') = readKey (skipWhiteSpaceAndGetc strm'') (* array-table *)
                                                  val strm'''' = expect (#"]", expect (#"]", strm'''))
                                              in SOME (ARRAY_TABLE key, skipWhiteSpaceOrComment strm'''')
                                              end
                     | _ => let val (key, strm'') = readKey (skipWhiteSpaceAndGetc strm') (* std-table *)
                                val strm''' = expect (#"]", strm'')
                            in SOME (STD_TABLE key, skipWhiteSpaceOrComment strm''')
                            end
                  )
                | r => let val (key, v, strm') = readKeyval (revPath, r) (* keyval *)
                       in SOME (KEYVAL (key, v), skipWhiteSpaceOrComment strm')
                       end
          (*: val insertKeyval : path * partial_table * key * key * value -> partial_table *)
          fun insertKeyval (revPath, pt, [], keys, v) = insert (revPath, pt, keys, v)
            | insertKeyval (revPath, pt, x0 :: xs, keys, v)
            = let fun go (accum, []) = List.revAppend (accum, [(x0, PARTIAL_TABLE (IMPLICIT_HEADER, insertKeyval (x0 :: revPath, [], xs, keys, v)))]) (* should not occur *)
                    | go (accum, (p as (key, pv)) :: ys)
                    = if x0 = key then
                          let val updated = case pv of
                                                LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                              | PARTIAL_TABLE (definedBy, pt') => PARTIAL_TABLE (definedBy, insertKeyval (x0 :: revPath, pt', xs, keys, v))
                                              | PARTIAL_ARRAY (last, rest) => PARTIAL_ARRAY (insertKeyval (x0 :: revPath, last, xs, keys, v), rest)
                          in List.revAppend (accum, (key, updated) :: ys)
                          end
                      else
                          go (p :: accum, ys)
              in go ([], pt)
              end
          fun insertTable (revPath, pt, [key])
            = let fun go (accum, []) = List.revAppend (accum, [(key, PARTIAL_TABLE (EXACT_HEADER, []))]) (* new table (explicit) *)
                    | go (accum, (p as (key', pv)) :: xs)
                        = if key = key' then
                              case pv of
                                  LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                | PARTIAL_TABLE (IMPLICIT_HEADER, pt') => List.revAppend (accum, (key', PARTIAL_TABLE (EXACT_HEADER, pt')) :: xs) (* convert implicit table into explicit one *)
                                | PARTIAL_TABLE (_, _) => raise DuplicateKey (List.revAppend (revPath, [key]))
                                | PARTIAL_ARRAY _ => raise DuplicateKey (List.revAppend (revPath, [key])) (* previous: array, now: table *)
                          else
                              go (p :: accum, xs)
              in go ([], pt)
              end
            | insertTable (revPath, pt, key :: keys)
            = let fun go (accum, []) = List.revAppend (accum, [(key, PARTIAL_TABLE (IMPLICIT_HEADER, insertTable (key :: revPath, [], keys)))]) (* new table (implicit) *)
                    | go (accum, (p as (key', pv)) :: xs)
                        = if key = key' then
                              let val updated = case pv of
                                                    LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                                  | PARTIAL_TABLE (definedBy, pt') => PARTIAL_TABLE (definedBy, insertTable (key :: revPath, pt', keys))
                                                  | PARTIAL_ARRAY (last, rest) => PARTIAL_ARRAY (insertTable (key :: revPath, last, keys), rest)
                              in List.revAppend (accum, (key', updated) :: xs)
                              end
                          else
                              go (p :: accum, xs)
              in go ([], pt)
              end
            | insertTable (_, _, []) = raise Match (* should not occur *)
          fun insertArrayTable (revPath, pt, [key])
            = let fun go (accum, []) = List.revAppend (accum, [(key, PARTIAL_ARRAY ([], []))]) (* new array table *)
                    | go (accum, (p as (key', pv)) :: xs)
                        = if key = key' then
                              case pv of
                                  LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                | PARTIAL_TABLE _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                | PARTIAL_ARRAY (last, rest) => List.revAppend (accum, (key, PARTIAL_ARRAY ([], last :: rest)) :: xs)
                          else
                              go (p :: accum, xs)
              in go ([], pt)
              end
            | insertArrayTable (revPath, pt, key :: keys)
            = let fun go (accum, []) = List.revAppend (accum, [(key, PARTIAL_TABLE (IMPLICIT_HEADER, insertArrayTable (key :: revPath, [], keys)))]) (* new table (implicit) *)
                    | go (accum, (p as (key', pv)) :: xs)
                        = if key = key' then
                              let val updated = case pv of
                                  LEAF _ => raise DuplicateKey (List.revAppend (revPath, [key]))
                                | PARTIAL_TABLE (definedBy, pt') => PARTIAL_TABLE (definedBy, insertArrayTable (key :: revPath, pt', keys))
                                | PARTIAL_ARRAY (last, rest) => PARTIAL_ARRAY (insertArrayTable (key :: revPath, last, keys), rest)
                              in List.revAppend (accum, (key, updated) :: xs)
                              end
                          else
                              go (p :: accum, xs)
              in go ([], pt)
              end
            | insertArrayTable (_, _, []) = raise Match (* should not occur *)
          fun parse (accum, revPath, prefix, strm)
            = case readExpression (revPath, strm) of
                  NONE => finalizeTable accum
                | SOME (ARRAY_TABLE key, strm') => parse (insertArrayTable ([], accum, key), "[]" :: List.rev key, key, strm')
                | SOME (STD_TABLE key, strm') => parse (insertTable ([], accum, key), List.rev key, key, strm')
                | SOME (KEYVAL (key, v), strm') => parse (insertKeyval (List.revAppend (key, revPath), accum, prefix, key, v), revPath, prefix, strm')
      in fn strm => parse ([], [], [], strm)
      end
end
