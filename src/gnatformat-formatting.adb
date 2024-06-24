--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Libadalang.Generic_API;

package body Gnatformat.Formatting is

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is (Format
         (Libadalang.Generic_API.To_Generic_Unit (Unit),
          Format_Options.Into (Ada_Language),
          Configuration));

   ------------
   -- Format --
   ------------

   function Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Format_Options : Prettier_Ada.Documents.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Document : constant Prettier_Ada.Documents.Document_Type :=
        Langkit_Support.Generic_API.Unparsing.Unparse_To_Prettier
          (Unit.Root, Configuration);

   begin
      return Prettier_Ada.Documents.Format (Document, Format_Options);
   end Format;

   ------------------
   -- Range_Format --
   ------------------

   function Range_Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Span           : Langkit_Support.Slocs.Source_Location_Range;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Range_Format_Result
   is
   begin
      raise Program_Error with "Not implemented yet";

      return
        (Langkit_Support.Slocs.No_Source_Location_Range,
         Langkit_Support.Generic_API.Analysis.No_Lk_Node,
         Ada.Strings.Unbounded.Null_Unbounded_String);
   end Range_Format;

   ------------------
   -- Range_Format --
   ------------------

   function Range_Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Span           : Langkit_Support.Slocs.Source_Location_Range;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Range_Format_Result
   is
   begin
      raise Program_Error with "Not implemented yet";

      return
        (Langkit_Support.Slocs.No_Source_Location_Range,
         Langkit_Support.Generic_API.Analysis.No_Lk_Node,
         Ada.Strings.Unbounded.Null_Unbounded_String);
   end Range_Format;

end Gnatformat.Formatting;
