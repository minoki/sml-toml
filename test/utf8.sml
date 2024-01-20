fun isValidUtf8 s =
  let
    val getc = ValidateUtf8.validatingReader Substring.getc
    fun go strm =
      case getc strm of
        NONE => true
      | SOME (_, strm') => go strm'
  in
    go (ValidateUtf8.mkValidatingStream (Substring.full s))
    handle ValidateUtf8.InvalidUtf8 => false
  end
fun isValidLeadByte c =
  let
    val getc = ValidateUtf8.validatingReader Substring.getc
  in
    ( getc (ValidateUtf8.mkValidatingStream (Substring.full (String.str c)))
    ; true
    )
    handle ValidateUtf8.InvalidUtf8 => false
  end
fun checkValidUtf8 (name, s) =
  if isValidUtf8 s then
    print (name ^ ": PASS\n")
  else
    ( TextIO.output (TextIO.stdErr, name ^ ": FAIL\n")
    ; OS.Process.exit OS.Process.failure
    )
fun checkInvalidUtf8 (name, s) =
  if not (isValidUtf8 s) then
    print (name ^ ": PASS\n")
  else
    ( TextIO.output (TextIO.stdErr, name ^ ": FAIL\n")
    ; OS.Process.exit OS.Process.failure
    )
fun checkInvalidLeadByte (name, c) =
  if not (isValidLeadByte c) then
    print (name ^ ": PASS\n")
  else
    ( TextIO.output (TextIO.stdErr, name ^ ": FAIL\n")
    ; OS.Process.exit OS.Process.failure
    )
val () = checkValidUtf8 ("valid/ascii", "foobar baz");
val () = checkValidUtf8 ("valid/U+0080", "\194\128");
val () = checkValidUtf8 ("valid/U+0300", "\204\128");
val () = checkValidUtf8 ("valid/U+3042", "\227\129\130");
val () = checkValidUtf8 ("valid/U+D7FF", "\237\159\191");
val () = checkValidUtf8 ("valid/U+E000", "\238\128\128");
val () = checkValidUtf8 ("valid/U+FFFF", "\239\191\191");
val () = checkValidUtf8 ("valid/U+10000", "\240\144\128\128")
val () = checkValidUtf8 ("valid/U+10FFFF", "\244\143\191\191")
val () = checkValidUtf8
  ( "valid/all"
  , "foobar baz U+0080:\194\128 U+0300:\204\128 U+3042:\227\129\130 U+D7FF:\237\159\191 U+E000:\238\128\128 U+FFFF:\239\191\191 U+10000:\240\144\128\128 U+10FFFF:\244\143\191\191"
  );
val () = checkInvalidLeadByte ("invalid/invalid lead byte 128", #"\128");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 191", #"\191");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 192", #"\192");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 193", #"\193");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 245", #"\245");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 246", #"\246");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 247", #"\247");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 248", #"\248");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 249", #"\249");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 250", #"\250");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 251", #"\251");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 252", #"\252");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 253", #"\253");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 254", #"\254");
val () = checkInvalidLeadByte ("invalid/invalid lead byte 255", #"\255");
val () = checkInvalidUtf8 ("invalid/invalid trailing byte 1", "\194\194");
val () = checkInvalidUtf8 ("invalid/invalid trailing byte 2", "\227\129a");
val () = checkInvalidUtf8 ("invalid/extra", "\194\128\128");
val () = checkInvalidUtf8 ("invalid/too long 1", "\192\128");
val () = checkInvalidUtf8 ("invalid/too long 2", "\193\128");
val () = checkInvalidUtf8 ("invalid/too long 3", "\224\128\128");
val () = checkInvalidUtf8 ("invalid/U+D800", "\237\160\128");
val () = checkInvalidUtf8 ("invalid/U+DFFF", "\237\191\191");
val () = checkInvalidUtf8 ("invalid/U+110000", "\244\144\128\128");
val () = checkInvalidUtf8 ("invalid/U+1FFFFF", "\247\191\191\191");
val () = checkInvalidUtf8 ("invalid/U+200000", "\248\136\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+400000", "\248\144\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+800000", "\248\160\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+1000000", "\249\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+2000000", "\250\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+3000000", "\251\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+4000000", "\252\132\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+8000000", "\252\136\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+10000000", "\252\144\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+20000000", "\252\160\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+40000000", "\253\160\128\128\128\128");
val () = checkInvalidUtf8 ("invalid/U+7FFFFFFF", "\253\191\191\191\191\191");
