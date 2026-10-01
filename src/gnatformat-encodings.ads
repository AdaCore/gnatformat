--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
--  Charset detection and conversion for source files.

with Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Gnatformat.Configuration;
with Gnatformat.Utils;

package Gnatformat.Encodings is

   type Source_Encoding (Determined : Boolean := True) is record
      case Determined is
         when True =>
            Charset : Ada.Strings.Unbounded.Unbounded_String;
            --  Charset name compatible with GNATCOLL.Iconv

            Has_BOM : Boolean := False;
            --  Whether the source starts with the byte order mark for Charset

         when False =>
            Reason : Ada.Strings.Unbounded.Unbounded_String;
            --  Human-readable reason why the encoding could not be determined
      end case;
   end record;

   type BOM_Kind is (UTF_8, UTF_16_LE, UTF_16_BE, UTF_32_LE, UTF_32_BE);
   --  Byte order marks that decide the charset of a source: the subset of
   --  GNAT.Byte_Order_Mark.BOM_Kind for which GNATCOLL.Iconv has a charset.

   package Optional_BOM_Kinds is new Gnatformat.Utils.Optional (BOM_Kind);
   subtype Optional_BOM_Kind is Optional_BOM_Kinds.Optional_Type;

   function Detect_From_Buffer (Buffer : String) return Source_Encoding;
   --  Detects the encoding of Buffer:
   --  1) if Buffer starts with a byte order mark, the byte order mark decides
   --     the charset (UTF-8, UTF-16 or UTF-32 variants);
   --  2) otherwise, if Buffer contains non-ASCII bytes (but no NUL bytes) and
   --     is valid UTF-8, UTF-8 is assumed;
   --  3) otherwise, iso-8859-1 is assumed if Buffer is plausible iso-8859-1
   --     text. This includes pure ASCII sources, for which the charset
   --     choice makes no difference.
   --
   --  The result is undetermined (Determined = False) when none of the above
   --  applies: Buffer contains NUL bytes (which suggests UTF-16 or UTF-32
   --  without a byte order mark), or bytes in the 16#80# .. 16#9F# range,
   --  which are C1 control characters that never appear in legitimate
   --  iso-8859-1 text (they are typically Windows-1252 punctuation or
   --  remnants of another encoding).

   function Detect_From_File (Path : String) return Source_Encoding;
   --  Reads the file at Path and detects its encoding (see
   --  Detect_From_Buffer).
   --  If the file cannot be read, iso-8859-1 is assumed: the caller is
   --  expected to hit (and report) the read error itself when loading the
   --  source.
   --  Only meant to be called when no charset is explicitly configured for
   --  the source, i.e. when Gnatformat.Configuration.Get_Charset returns an
   --  unset result.

   function Read_BOM_From_File (Path : String) return Optional_BOM_Kind;
   --  The byte order mark the file at Path starts with, if it is one of
   --  BOM_Kind. Unset if the file cannot be read: the caller is expected to
   --  hit (and report) the read error itself when loading the source.

   Incompatible_BOM_Error : exception;
   --  Raised by Check_BOM (and so by Resolve_Encoding) when a source starts
   --  with a byte order mark incompatible with its configured charset. The
   --  exception message is a complete, user-facing explanation.

   procedure Check_BOM
     (Configured_Charset : Ada.Strings.Unbounded.Unbounded_String;
      BOM                : BOM_Kind);
   --  Checks that BOM, which a source starts with, is compatible with the
   --  charset explicitly configured for it: either Configured_Charset is
   --  BOM's charset (e.g. "utf-16le" for a UTF-16 little endian byte order
   --  mark), or Configured_Charset is the UTF-16 or UTF-32 charset without a
   --  byte order ("utf-16" or "utf-32") and BOM gives the byte order.
   --  Raises Incompatible_BOM_Error otherwise (e.g. for a UTF-16 little
   --  endian byte order mark with "utf-16be", or any with "iso-8859-1").

   function Encoding_With_BOM (BOM : BOM_Kind) return Source_Encoding;
   --  Encoding of a source starting with BOM: BOM's charset with Has_BOM set,
   --  so that the byte order mark is stripped when parsing the source and
   --  preserved when encoding the formatted source.

   Undetermined_Encoding_Error : exception;
   --  Raised by Resolve_Encoding when the encoding of a source cannot be
   --  determined. The exception message is the reason.

   function Resolve_Encoding
     (Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Source         : GNATCOLL.VFS.Virtual_File) return Source_Encoding;
   --  Effective encoding of Source: the charset explicitly configured for it
   --  in Format_Options if any (see Check_BOM), otherwise the one
   --  detected from its contents (see Detect_From_File). The result is
   --  always determined: raises Undetermined_Encoding_Error when the encoding
   --  cannot be determined, with the reason as the exception message, and
   --  Incompatible_BOM_Error when the source starts with a byte order mark
   --  incompatible with its configured charset.

   function Parsing_Charset (Encoding : Source_Encoding) return String;
   --  Charset to use to parse a source with the given Encoding, which must
   --  be determined.
   --  Empty when Encoding.Has_BOM is set, so that Libadalang reads and strips
   --  the byte order mark itself, otherwise Encoding.Charset.

   function Decode
     (Text : Ada.Strings.Unbounded.Unbounded_String; Charset : String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Converts Text, encoded in Charset, into its UTF-8 equivalent.
   --  GNATCOLL.Iconv does the conversion, so Charset must be a charset it
   --  supports. Raises GNATCOLL.Iconv exceptions if the conversion fails.

   function Encode
     (Text : Ada.Strings.Unbounded.Unbounded_String; Charset : String)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Converts Text, assumed to be UTF-8 encoded, into its Charset equivalent.
   --  GNATCOLL.Iconv does the conversion, so Charset must be a charset it
   --  supports. Raises GNATCOLL.Iconv exceptions if the conversion fails.

   function Encode
     (Utf_8_Text : Ada.Strings.Unbounded.Unbounded_String;
      Encoding   : Source_Encoding)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Converts Utf_8_Text, assumed to be UTF-8 encoded, into Encoding.Charset
   --  and prepends the byte order mark if Encoding.Has_BOM is set.
   --  Encoding must be determined.

end Gnatformat.Encodings;
