--
--  Copyright (C) 2024-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Gnatformat.Command_Line.Configuration is

   ---------
   -- Get --
   ---------

   function Get return Gnatformat.Configuration.Format_Options_Type is

      use Gnatformat.Configuration;
      use Override_Layout;

      function To_Optional_Files_Vector
        (Value : Override_Layout.Result_Array)
         return Gnatformat.Configuration.Optional_Files_Vector;
      --  Converts Override_Layout.Result_Array to
      --  Gnatformat.Configuration.Optional_Files_Vector.

      ------------------------------
      -- To_Optional_Files_Vector --
      ------------------------------

      function To_Optional_Files_Vector
        (Value : Override_Layout.Result_Array)
         return Gnatformat.Configuration.Optional_Files_Vector is
      begin
         if Value = Override_Layout.No_Results then
            return
              Gnatformat.Configuration.Optional_Files_Vector'(Is_Set => False);
         end if;

         return
           Gnatformat.Configuration.Optional_Files_Vector'
             (Is_Set => True, Value => [for V of Value => V]);

      end To_Optional_Files_Vector;

      Format_Options_Builder :
        Gnatformat.Configuration.Format_Options_Builder_Type :=
          Gnatformat.Configuration.Create_Format_Options_Builder;

      Charset_Value                  :
        constant Gnatformat.Configuration.Optional_Unbounded_String :=
          Charset.Get;
      Width_Value                    :
        constant Gnatformat.Configuration.Optional_Positive := Width.Get;
      Indentation_Value              :
        constant Gnatformat.Configuration.Optional_Positive := Indentation.Get;
      Indentation_Kind_Value         :
        constant Gnatformat.Configuration.Optional_Indentation_Kind :=
          Indentation_Kind.Get;
      Indentation_Continuation_Value :
        constant Gnatformat.Configuration.Optional_Positive :=
          Indentation_Continuation.Get;
      End_Of_Line_Value              :
        constant Gnatformat.Configuration.Optional_End_Of_Line_Kind :=
          End_Of_Line.Get;
      Keyword_Casing_Value           :
        constant Gnatformat.Configuration.Optional_Keyword_Casing_Kind :=
          Keyword_Casing.Get;
      Ignore_Value                   :
        constant Gnatformat.Configuration.Optional_Unbounded_String :=
          Ignore.Get;

      Layout_Value : constant Optional_Layout := Layout.Get;

      Override_Layout_Value : constant Optional_Files_Vector :=
        To_Optional_Files_Vector (Override_Layout.Get);

   begin
      if Charset_Value.Is_Set then
         Gnatformat.Configuration.With_Charset
           (Self     => Format_Options_Builder,
            Charset  => Charset_Value.Value,
            Language => Ada_Language);
      end if;
      if Width_Value.Is_Set then
         Gnatformat.Configuration.With_Width
           (Self     => Format_Options_Builder,
            Width    => Width_Value.Value,
            Language => Ada_Language);
      end if;
      if Indentation_Value.Is_Set then
         Format_Options_Builder.With_Indentation
           (Indentation => Indentation_Value.Value, Language => Ada_Language);
      end if;
      if Indentation_Kind_Value.Is_Set then
         Format_Options_Builder.With_Indentation_Kind
           (Indentation_Kind => Indentation_Kind_Value.Value,
            Language         => Ada_Language);
      end if;
      if Indentation_Continuation_Value.Is_Set then
         Format_Options_Builder.With_Indentation_Continuation
           (Indentation_Continuation => Indentation_Continuation_Value.Value,
            Language                 => Ada_Language);
      end if;
      if End_Of_Line_Value.Is_Set then
         Format_Options_Builder.With_End_Of_Line
           (End_Of_Line => End_Of_Line_Value.Value, Language => Ada_Language);
      end if;
      if Keyword_Casing_Value.Is_Set then
         Format_Options_Builder.With_Keyword_Casing
           (Keyword_Casing => Keyword_Casing_Value.Value,
            Language       => Ada_Language);
      end if;
      if Ignore_Value.Is_Set then
         Format_Options_Builder.With_Ignore
           (GNATCOLL.VFS.Create_From_UTF8
              (Ada.Strings.Unbounded.To_String (Ignore_Value.Value)));
      end if;
      if Layout_Value.Is_Set then
         Format_Options_Builder.With_Layout
           (Layout => Layout_Value.Value, Language => Ada_Language);
      end if;
      if Override_Layout_Value.Is_Set then
         Format_Options_Builder.With_Override_Layout
           (Override_Layout_Files => Override_Layout_Value,
            Language              => Ada_Language);
      end if;

      return Format_Options_Builder.Build;
   end Get;

end Gnatformat.Command_Line.Configuration;
