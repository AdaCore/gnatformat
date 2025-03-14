--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Gnatformat.Command_Line.Configuration is

   ---------
   -- Get --
   ---------

   function Get return Gnatformat.Configuration.Format_Options_Type is
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
      Ignore_Value                   :
        constant Gnatformat.Configuration.Optional_Unbounded_String :=
          Ignore.Get;

   begin
      if Charset_Value.Is_Set then
         Gnatformat.Configuration.With_Charset
           (Self     => Format_Options_Builder,
            Charset  => Charset_Value.Value,
            Language => Ada_Language);
      end if;
      if Width_Value.Is_Set then
         Gnatformat.Configuration.With_Width
           (Self => Format_Options_Builder,
            Width => Width_Value.Value,
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
      if Ignore_Value.Is_Set then
         Format_Options_Builder.With_Ignore
           (GNATCOLL.VFS.Create_From_UTF8
              (Ada.Strings.Unbounded.To_String (Ignore_Value.Value)));
      end if;

      return Format_Options_Builder.Build;
   end Get;

end Gnatformat.Command_Line.Configuration;
