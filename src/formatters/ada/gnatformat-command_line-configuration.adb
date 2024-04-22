--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Gnatformat.Command_Line.Configuration is

   ---------
   -- Get --
   ---------

   function Get return Gnatformat.Configuration.Format_Options_Type
   is
      Format_Options_Builder :
        Gnatformat.Configuration.Format_Options_Builder_Type
          := Gnatformat.Configuration.Create_Format_Options_Builder;

      Width_Value                              :
        constant Gnatformat.Configuration.Optional_Positive         :=
          Width.Get;
      Indentation_Value                        :
        constant Gnatformat.Configuration.Optional_Positive         :=
          Indentation.Get;
      Indentation_Kind_Value                   :
        constant Gnatformat.Configuration.Optional_Indentation_Kind :=
          Indentation_Kind.Get;
      Continuation_Line_Indentation_Value      :
        constant Gnatformat.Configuration.Optional_Positive         :=
          Continuation_Line_Indentation.Get;
      End_Of_Line_Value                        :
        constant Gnatformat.Configuration.Optional_End_Of_Line_Kind :=
          End_Of_Line.Get;

   begin
      if Width_Value.Is_Set then
         Format_Options_Builder.With_Width
           (Width    => Width_Value.Value,
            Language => Ada_Language);
      end if;
      if Indentation_Value.Is_Set then
         Format_Options_Builder.With_Indentation
           (Indentation => Indentation_Value.Value,
            Language    => Ada_Language);
      end if;
      if Indentation_Kind_Value.Is_Set then
         Format_Options_Builder.With_Indentation_Kind
           (Indentation_Kind => Indentation_Kind_Value.Value,
            Language         => Ada_Language);
      end if;
      if Continuation_Line_Indentation_Value.Is_Set then
         Format_Options_Builder.With_Continuation_Line_Indentation
           (Continuation_Line_Indentation =>
              Continuation_Line_Indentation_Value.Value,
            Language                      => Ada_Language);
      end if;
      if End_Of_Line_Value.Is_Set then
         Format_Options_Builder.With_End_Of_Line
           (End_Of_Line => End_Of_Line_Value.Value,
            Language    => Ada_Language);
      end if;

      return Format_Options_Builder.Build;
   end Get;

end Gnatformat.Command_Line.Configuration;
