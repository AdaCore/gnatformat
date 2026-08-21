--
--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Characters.Handling;
pragma Warnings (Off, "-gnatwi");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "-gnatwi");

with GNAT.Byte_Order_Mark;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;

package body Gnatformat.Encodings is

   function "+" (Source : String) return Ada.Strings.Unbounded.Unbounded_String
   renames Ada.Strings.Unbounded.To_Unbounded_String;

   function BOM (Encoding : Source_Encoding) return String;
   --  The byte order mark bytes for Encoding.Charset if Encoding.Has_BOM is
   --  set, otherwise an empty string.

   function BOM_Charset (BOM : BOM_Kind) return String;
   --  The GNATCOLL.Iconv charset whose byte order mark is BOM

   function BOM_Name (BOM : BOM_Kind) return String;
   --  Human-readable name of BOM, with its indefinite article, for messages
   --  (e.g. "a UTF-16 little endian")

   function Is_BOM_Compatible
     (BOM : BOM_Kind; Charset : String) return Boolean;
   --  Whether BOM is compatible with the explicitly configured Charset (see
   --  Check_BOM)

   function Convert
     (Text      : Ada.Strings.Unbounded.Unbounded_String;
      From_Code : String;
      To_Code   : String) return Ada.Strings.Unbounded.Unbounded_String;
   --  Converts Text from the From_Code charset into the To_Code charset
   --  using GNATCOLL.Iconv.

   function Normalized_Charset (Charset : String) return String;
   --  Charset in lower case and without '-' and '_' characters, so that e.g.
   --  "UTF-16LE", "utf-16le" and "utf16le" all give "utf16le"

   function Is_UTF_8 (Charset : String) return Boolean
   is (Ada.Characters.Handling.To_Lower (Charset) in "utf-8" | "utf8");
   --  Checks if Charset is an alias of the UTF-8 charset

   function Read_BOM (Buffer : String) return Optional_BOM_Kind;
   --  The byte order mark Buffer starts with, if it is one of BOM_Kind

   ---------
   -- BOM --
   ---------

   function BOM (Encoding : Source_Encoding) return String is
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      if not Encoding.Has_BOM then
         return "";
      end if;

      if Encoding.Charset = GNATCOLL.Iconv.UTF8 then
         return
           [Character'Val (16#EF#),
            Character'Val (16#BB#),
            Character'Val (16#BF#)];

      elsif Encoding.Charset = GNATCOLL.Iconv.UTF16LE then
         return [Character'Val (16#FF#), Character'Val (16#FE#)];

      elsif Encoding.Charset = GNATCOLL.Iconv.UTF16BE then
         return [Character'Val (16#FE#), Character'Val (16#FF#)];

      elsif Encoding.Charset = GNATCOLL.Iconv.UTF32LE then
         return
           [Character'Val (16#FF#),
            Character'Val (16#FE#),
            Character'Val (16#00#),
            Character'Val (16#00#)];

      elsif Encoding.Charset = GNATCOLL.Iconv.UTF32BE then
         return
           [Character'Val (16#00#),
            Character'Val (16#00#),
            Character'Val (16#FE#),
            Character'Val (16#FF#)];

      else
         return "";
      end if;
   end BOM;

   -----------------
   -- BOM_Charset --
   -----------------

   function BOM_Charset (BOM : BOM_Kind) return String
   is (case BOM is
         when UTF_8     => GNATCOLL.Iconv.UTF8,
         when UTF_16_LE => GNATCOLL.Iconv.UTF16LE,
         when UTF_16_BE => GNATCOLL.Iconv.UTF16BE,
         when UTF_32_LE => GNATCOLL.Iconv.UTF32LE,
         when UTF_32_BE => GNATCOLL.Iconv.UTF32BE);

   --------------
   -- BOM_Name --
   --------------

   function BOM_Name (BOM : BOM_Kind) return String
   is (case BOM is
         when UTF_8     => "a UTF-8",
         when UTF_16_LE => "a UTF-16 little endian",
         when UTF_16_BE => "a UTF-16 big endian",
         when UTF_32_LE => "a UTF-32 little endian",
         when UTF_32_BE => "a UTF-32 big endian");

   ---------------
   -- Check_BOM --
   ---------------

   procedure Check_BOM
     (Configured_Charset : Ada.Strings.Unbounded.Unbounded_String;
      BOM                : BOM_Kind) is
   begin
      --  An incompatible byte order mark means that the configured charset
      --  is wrong for this source: decoding it would fail (or worse,
      --  silently produce garbage), so refuse to go further.

      if not Is_BOM_Compatible
               (BOM, Ada.Strings.Unbounded.To_String (Configured_Charset))
      then
         raise Incompatible_BOM_Error
           with
             "The source starts with "
             & BOM_Name (BOM)
             & " byte order mark, which contradicts the configured "
             & "charset """
             & Ada.Strings.Unbounded.To_String (Configured_Charset)
             & """. Configure a compatible charset instead, such as """
             & BOM_Charset (BOM)
             & """.";
      end if;
   end Check_BOM;

   -------------
   -- Convert --
   -------------

   function Convert
     (Text      : Ada.Strings.Unbounded.Unbounded_String;
      From_Code : String;
      To_Code   : String) return Ada.Strings.Unbounded.Unbounded_String
   is
      Text_Access : Ada.Strings.Unbounded.Aux.Big_String_Access;
      Text_Length : Natural;

   begin
      Ada.Strings.Unbounded.Aux.Get_String (Text, Text_Access, Text_Length);

      if Text_Length = 0 then
         return Text;
      end if;

      return
        Ada.Strings.Unbounded.To_Unbounded_String
          (GNATCOLL.Iconv.Iconv
             (Input     => Text_Access.all (1 .. Text_Length),
              To_Code   => To_Code,
              From_Code => From_Code));
   end Convert;

   ------------
   -- Decode --
   ------------

   function Decode
     (Text : Ada.Strings.Unbounded.Unbounded_String; Charset : String)
      return Ada.Strings.Unbounded.Unbounded_String
   is (if Is_UTF_8 (Charset)
       then Text
       else
         Convert (Text, From_Code => Charset, To_Code => GNATCOLL.Iconv.UTF8));

   ------------------------
   -- Detect_From_Buffer --
   ------------------------

   function Detect_From_Buffer (Buffer : String) return Source_Encoding is
   begin
      --  Start by trying to detect a known byte order mark

      declare
         BOM : constant Optional_BOM_Kind := Read_BOM (Buffer);

      begin
         if BOM.Is_Set then
            return Encoding_With_BOM (BOM.Value);
         end if;
      end;

      --  No byte order mark: a single pass over Buffer checks that it is
      --  valid UTF-8 and collects the byte-level facts that decide the
      --  result below.

      declare
         Has_NUL : Boolean := False;
         --  Whether Buffer contains NUL bytes. Text sources never contain
         --  them, even when the rest of the buffer is valid UTF-8 or
         --  plausible iso-8859-1 text.

         Has_C1_Controls : Boolean := False;
         --  Whether Buffer contains bytes in the 16#80# .. 16#9F# range,
         --  which are C1 control characters that never appear in legitimate
         --  iso-8859-1 text.

         Non_ASCII : Boolean := False;
         --  Whether Buffer contains non-ASCII bytes

         Valid_UTF_8 : Boolean := True;
         --  Whether Buffer is valid UTF-8, following the constraints of
         --  RFC 3629: no overlong sequences, no surrogates and no codepoints
         --  above 10FFFF.

         Remaining_Continuations : Natural := 0;
         --  Number of continuation bytes still expected for the current
         --  UTF-8 sequence

         Next_Low  : Character := Character'Val (16#80#);
         Next_High : Character := Character'Val (16#BF#);
         --  Bounds for the next expected continuation byte: constrained for
         --  the byte right after the lead (to reject overlong sequences,
         --  surrogates and codepoints above 10FFFF), then reset to the plain
         --  continuation bounds for the rest of the sequence.

      begin
         for Byte of Buffer loop
            Has_NUL := @ or else Byte = Character'Val (16#00#);
            Has_C1_Controls :=
              @
              or else Byte in Character'Val (16#80#) .. Character'Val (16#9F#);
            Non_ASCII := @ or else Byte >= Character'Val (16#80#);

            if Valid_UTF_8 then
               if Remaining_Continuations > 0 then
                  if Byte in Next_Low .. Next_High then
                     Remaining_Continuations := @ - 1;
                     Next_Low := Character'Val (16#80#);
                     Next_High := Character'Val (16#BF#);
                  else
                     Valid_UTF_8 := False;
                  end if;

               else
                  case Byte is
                     when Character'Val (16#00#) .. Character'Val (16#7F#) =>
                        null;

                     when Character'Val (16#C2#) .. Character'Val (16#DF#) =>
                        Remaining_Continuations := 1;

                     when Character'Val (16#E0#)                           =>
                        Remaining_Continuations := 2;
                        Next_Low := Character'Val (16#A0#);

                     when Character'Val (16#E1#) .. Character'Val (16#EC#)
                        | Character'Val (16#EE#) .. Character'Val (16#EF#) =>
                        Remaining_Continuations := 2;

                     when Character'Val (16#ED#)                           =>
                        Remaining_Continuations := 2;
                        Next_High := Character'Val (16#9F#);

                     when Character'Val (16#F0#)                           =>
                        Remaining_Continuations := 3;
                        Next_Low := Character'Val (16#90#);

                     when Character'Val (16#F1#) .. Character'Val (16#F3#) =>
                        Remaining_Continuations := 3;

                     when Character'Val (16#F4#)                           =>
                        Remaining_Continuations := 3;
                        Next_High := Character'Val (16#8F#);

                     when others                                           =>
                        --  16#80# .. 16#C1# and 16#F5# .. 16#FF# cannot lead
                        --  a UTF-8 sequence.

                        Valid_UTF_8 := False;
                  end case;
               end if;
            end if;
         end loop;

         --  A sequence truncated by the end of Buffer is invalid

         Valid_UTF_8 := Valid_UTF_8 and then Remaining_Continuations = 0;

         if Has_NUL then
            return
              (Determined => False,
               Reason     =>
                 +("it contains NUL bytes, which suggests UTF-16 or "
                   & "UTF-32 without a byte order mark"));
         end if;

         if Valid_UTF_8 then
            --  Pure ASCII text is valid UTF-8, but the charset choice makes
            --  no difference for it: assume iso-8859-1.

            return
              (Determined => True,
               Charset    =>
                 (if Non_ASCII
                  then +GNATCOLL.Iconv.UTF8
                  else +Gnatformat.Configuration.Default_Charset),
               Has_BOM    => False);
         end if;

         --  Not valid UTF-8: assume iso-8859-1 unless the buffer contains C1
         --  control characters, which never appear in legitimate iso-8859-1
         --  text.

         if Has_C1_Controls then
            return
              (Determined => False,
               Reason     =>
                 +("it is neither valid UTF-8 nor plausible "
                   & Gnatformat.Configuration.Default_Charset
                   & " text"));
         end if;

         return
           (Determined => True,
            Charset    => +Gnatformat.Configuration.Default_Charset,
            Has_BOM    => False);
      end;
   end Detect_From_Buffer;

   ----------------------
   -- Detect_From_File --
   ----------------------

   function Detect_From_File (Path : String) return Source_Encoding is
      File   : GNATCOLL.Mmap.Mapped_File;
      Region : GNATCOLL.Mmap.Mapped_Region;

   begin
      begin
         File := GNATCOLL.Mmap.Open_Read (Path);

      exception
         when others =>
            return
              (Determined => True,
               Charset    => +Gnatformat.Configuration.Default_Charset,
               Has_BOM    => False);
      end;

      Region := GNATCOLL.Mmap.Read (File);

      declare
         Buffer : String (1 .. GNATCOLL.Mmap.Last (Region))
         with Import, Address => GNATCOLL.Mmap.Data (Region).all'Address;

         Result : constant Source_Encoding := Detect_From_Buffer (Buffer);

      begin
         GNATCOLL.Mmap.Free (Region);
         GNATCOLL.Mmap.Close (File);

         return Result;
      end;
   end Detect_From_File;

   ------------
   -- Encode --
   ------------

   function Encode
     (Text : Ada.Strings.Unbounded.Unbounded_String; Charset : String)
      return Ada.Strings.Unbounded.Unbounded_String
   is (if Is_UTF_8 (Charset)
       then Text
       else
         Convert (Text, From_Code => GNATCOLL.Iconv.UTF8, To_Code => Charset));

   ------------
   -- Encode --
   ------------

   function Encode
     (Utf_8_Text : Ada.Strings.Unbounded.Unbounded_String;
      Encoding   : Source_Encoding)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      return
        BOM (Encoding)
        & Encode
            (Utf_8_Text, Ada.Strings.Unbounded.To_String (Encoding.Charset));
   end Encode;

   -----------------------
   -- Encoding_With_BOM --
   -----------------------

   function Encoding_With_BOM (BOM : BOM_Kind) return Source_Encoding
   is (Determined => True, Charset => +BOM_Charset (BOM), Has_BOM => True);

   -----------------------
   -- Is_BOM_Compatible --
   -----------------------

   function Is_BOM_Compatible (BOM : BOM_Kind; Charset : String) return Boolean
   is
      Normalized : constant String := Normalized_Charset (Charset);

   begin
      return
        (case BOM is
           when UTF_8     => Normalized = "utf8",
           when UTF_16_LE => Normalized in "utf16" | "utf16le",
           when UTF_16_BE => Normalized in "utf16" | "utf16be",
           when UTF_32_LE => Normalized in "utf32" | "utf32le",
           when UTF_32_BE => Normalized in "utf32" | "utf32be");
   end Is_BOM_Compatible;

   ------------------------
   -- Normalized_Charset --
   ------------------------

   function Normalized_Charset (Charset : String) return String is
      Result : String (1 .. Charset'Length);
      Last   : Natural := 0;

   begin
      for C of Charset loop
         if C not in '-' | '_' then
            Last := @ + 1;
            Result (Last) := Ada.Characters.Handling.To_Lower (C);
         end if;
      end loop;

      return Result (1 .. Last);
   end Normalized_Charset;

   ---------------------
   -- Parsing_Charset --
   ---------------------

   function Parsing_Charset (Encoding : Source_Encoding) return String
   is (if Encoding.Has_BOM
       then ""
       else Ada.Strings.Unbounded.To_String (Encoding.Charset));

   --------------
   -- Read_BOM --
   --------------

   function Read_BOM (Buffer : String) return Optional_BOM_Kind is
      BOM_Length : Natural;
      BOM        : GNAT.Byte_Order_Mark.BOM_Kind;

   begin
      GNAT.Byte_Order_Mark.Read_BOM (Buffer, BOM_Length, BOM);

      case BOM is
         when GNAT.Byte_Order_Mark.UTF8_All =>
            return (Is_Set => True, Value => UTF_8);

         when GNAT.Byte_Order_Mark.UTF16_LE =>
            return (Is_Set => True, Value => UTF_16_LE);

         when GNAT.Byte_Order_Mark.UTF16_BE =>
            return (Is_Set => True, Value => UTF_16_BE);

         when GNAT.Byte_Order_Mark.UTF32_LE =>
            return (Is_Set => True, Value => UTF_32_LE);

         when GNAT.Byte_Order_Mark.UTF32_BE =>
            return (Is_Set => True, Value => UTF_32_BE);

         when GNAT.Byte_Order_Mark.UCS4_BE
            | GNAT.Byte_Order_Mark.UCS4_LE
            | GNAT.Byte_Order_Mark.UCS4_2143
            | GNAT.Byte_Order_Mark.UCS4_3412
            | GNAT.Byte_Order_Mark.Unknown  =>
            --  The UCS-4 cases are only reported with XML_Support

            return Optional_BOM_Kinds.None;
      end case;
   end Read_BOM;

   ------------------------
   -- Read_BOM_From_File --
   ------------------------

   function Read_BOM_From_File (Path : String) return Optional_BOM_Kind is
      File   : GNATCOLL.Mmap.Mapped_File;
      Region : GNATCOLL.Mmap.Mapped_Region;

   begin
      begin
         File := GNATCOLL.Mmap.Open_Read (Path);

      exception
         when others =>
            return Optional_BOM_Kinds.None;
      end;

      --  The longest byte order mark (UTF-32) is 4 bytes long

      Region := GNATCOLL.Mmap.Read (File, Offset => 0, Length => 4);

      declare
         Buffer : String (1 .. GNATCOLL.Mmap.Last (Region))
         with Import, Address => GNATCOLL.Mmap.Data (Region).all'Address;

         BOM : constant Optional_BOM_Kind := Read_BOM (Buffer);

      begin
         GNATCOLL.Mmap.Free (Region);
         GNATCOLL.Mmap.Close (File);

         return BOM;
      end;
   end Read_BOM_From_File;

   ----------------------
   -- Resolve_Encoding --
   ----------------------

   function Resolve_Encoding
     (Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Source         : GNATCOLL.VFS.Virtual_File) return Source_Encoding
   is
      Configured_Charset :
        constant Gnatformat.Configuration.Optional_Unbounded_String :=
          Gnatformat.Configuration.Get_Charset
            (Format_Options, Source.Display_Base_Name);

   begin
      if Configured_Charset.Is_Set then
         declare
            BOM : constant Optional_BOM_Kind :=
              Read_BOM_From_File (Source.Display_Full_Name);

         begin
            if BOM.Is_Set then
               Check_BOM (Configured_Charset.Value, BOM.Value);

               return Encoding_With_BOM (BOM.Value);
            end if;

            return
              (Determined => True,
               Charset    => Configured_Charset.Value,
               Has_BOM    => False);
         end;
      end if;

      declare
         Encoding : constant Source_Encoding :=
           Detect_From_File (Source.Display_Full_Name);

      begin
         if not Encoding.Determined then
            raise Undetermined_Encoding_Error
              with Ada.Strings.Unbounded.To_String (Encoding.Reason);
         end if;

         return Encoding;
      end;
   end Resolve_Encoding;

end Gnatformat.Encodings;
