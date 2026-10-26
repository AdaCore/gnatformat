--
--  Copyright (C) 2024-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Gnatformat.Project;

with Langkit_Support.Diagnostics;

package body Gnatformat.Command_Line.Configuration is

   ---------------------------------
   -- Get_Unparsing_Configuration --
   ---------------------------------

   function Get_Unparsing_Configuration
     (Cache           :
        in out Gnatformat.Configuration.Unparsing_Configuration_Cache_Type;
      Source_Filename : String;
      Format_Options  : Gnatformat.Configuration.Format_Options_Type;
      Writer          : in out Abstract_Writers.Abstract_Writer'Class)
      return Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration
   is
      use Langkit_Support.Generic_API.Unparsing;
      use type GNATCOLL.VFS.Virtual_File;

      Diagnostics : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Base_File   : constant GNATCOLL.VFS.Virtual_File :=
        Unparsing_Configuration.Get;
      Config      :
        constant Langkit_Support
                   .Generic_API
                   .Unparsing
                   .Unparsing_Configuration :=
          Cache.Get (Source_Filename, Format_Options, Diagnostics);

   begin
      --  Keep the same failure policy for all CLI formatting modes: loading
      --  diagnostics make the configuration unusable, even if the engine
      --  returned a configuration alongside them.

      if Config = No_Unparsing_Configuration or else not Diagnostics.Is_Empty
      then
         Gnatformat.Project.Set_General_Failed;
         Writer.Print_Error
           ((if Source_Filename = ""
             then "Could not load unparsing configuration"
             else
               "--  "
               & Source_Filename
               & " failed: could not load unparsing configuration"));

         --  Engine diagnostics do not always name the configuration file.
         --  Show the custom base and the effective overrides, including
         --  overrides resolved from project attributes for this source.

         if Base_File /= GNATCOLL.VFS.No_File then
            Writer.Print_Error
              ("Unparsing configuration: " & Base_File.Display_Full_Name,
               False);
         end if;
         for File of
           Gnatformat.Configuration.Get_Override_Layout
             (Format_Options, Source_Filename)
         loop
            Writer.Print_Error
              ("Layout override: " & File.Display_Full_Name, False);
         end loop;

         --  Preserve every unparsing engine message and its available source
         --  location. Use the error writer so diagnostics stays apart of
         --  formatted stdout.

         for Diagnostic of Diagnostics loop
            Writer.Print_Error
              (Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic),
               False);
         end loop;
         return No_Unparsing_Configuration;
      end if;

      return Config;
   end Get_Unparsing_Configuration;

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
      Identifier_Casing_Value        :
        constant Gnatformat.Configuration.Optional_Identifier_Casing_Kind :=
          Identifier_Casing.Get;
      Ignore_Value                   :
        constant Gnatformat.Configuration.Optional_Unbounded_String :=
          Ignore.Get;

      Layout_Value : constant Optional_Layout := Layout.Get;

      Override_Layout_Value : constant Optional_Files_Vector :=
        To_Optional_Files_Vector (Override_Layout.Get);

   begin
      if Charset_Value.Is_Set then
         Gnatformat.Configuration.With_Charset
           (Self => Format_Options_Builder, Charset => Charset_Value.Value);
      end if;
      if Width_Value.Is_Set then
         Gnatformat.Configuration.With_Width
           (Self => Format_Options_Builder, Width => Width_Value.Value);
      end if;
      if Indentation_Value.Is_Set then
         Format_Options_Builder.With_Indentation
           (Indentation => Indentation_Value.Value);
      end if;
      if Indentation_Kind_Value.Is_Set then
         Format_Options_Builder.With_Indentation_Kind
           (Indentation_Kind => Indentation_Kind_Value.Value);
      end if;
      if Indentation_Continuation_Value.Is_Set then
         Format_Options_Builder.With_Indentation_Continuation
           (Indentation_Continuation => Indentation_Continuation_Value.Value);
      end if;
      if End_Of_Line_Value.Is_Set then
         Format_Options_Builder.With_End_Of_Line
           (End_Of_Line => End_Of_Line_Value.Value);
      end if;
      if Keyword_Casing_Value.Is_Set then
         Format_Options_Builder.With_Keyword_Casing
           (Keyword_Casing => Keyword_Casing_Value.Value);
      end if;
      if Identifier_Casing_Value.Is_Set then
         Format_Options_Builder.With_Identifier_Casing
           (Identifier_Casing => Identifier_Casing_Value.Value);
      end if;
      if Ignore_Value.Is_Set then
         Format_Options_Builder.With_Ignore
           (GNATCOLL.VFS.Create_From_UTF8
              (Ada.Strings.Unbounded.To_String (Ignore_Value.Value)));
      end if;
      if Layout_Value.Is_Set then
         Format_Options_Builder.With_Layout (Layout => Layout_Value.Value);
      end if;
      if Override_Layout_Value.Is_Set then
         Format_Options_Builder.With_Override_Layout
           (Override_Layout_Files => Override_Layout_Value);
      end if;

      return Format_Options_Builder.Build;
   end Get;

end Gnatformat.Command_Line.Configuration;
