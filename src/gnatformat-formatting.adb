--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Libadalang.Generic_API;
with Langkit_Support.Text;

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
   ------------------------------------------------------------------------

      function Diagnostics_Array_To_Vector
        (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector;
      --  Convert diagnostics array into dignostics vector

      -----------------------------------
      --  Diagnostics_Array_To_Vector  --
      -----------------------------------

      function Diagnostics_Array_To_Vector
        (Arr : Diagnostics_Array) return Diagnostics_Vectors.Vector
      is
         V : Diagnostics_Vectors.Vector := Diagnostics_Vectors.Empty_Vector;
      begin
         for I in Arr'Range loop
            Append
              (Diagnostics => V,
               Sloc_Range  => Arr (I).Sloc_Range,
               Message     => Langkit_Support.Text.To_Text (Arr (I).Message));
         end loop;
         return V;
      end Diagnostics_Array_To_Vector;

   --------------
   --  Format  --
   --------------

   function Format
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config_File : GNATCOLL.VFS.Virtual_File)
      return Formatted_Edits
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use Ada.Strings.Unbounded;
      use Langkit_Support.Generic_API;

      SL_Root         : constant Source_Location_Range :=
        Unit.Root.Sloc_Range;
      Unparse_Diag    : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Unparsing_Conf  : constant Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Load_Unparsing_Configuration
          (Unparsing_Config_File, Unparse_Diag);

      Formatted_Str   : constant Unbounded_String :=
        Format (Unit => Unit,
                Format_Options => Options,
                Configuration  => Unparsing_Conf);
      Diagnostics_V   : constant Diagnostics_Vectors.Vector :=
        Diagnostics_Array_To_Vector (Unit.Diagnostics);

   begin
      return Formatted_Edits'
        (Unit => Unit,
         Edit =>
           Text_Edit'(Location => SL_Root,
                      Text     => Formatted_Str),
         Formatted   => Unit.Root,
         Diagnostics => Diagnostics_V);
   end Format;

   ------------------------
   --  Format_Selection  --
   ------------------------

   function Format_Selection
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Input_Selection_Range : Langkit_Support.Slocs.Source_Location_Range;
      Options               : Gnatformat.Configuration.Format_Options_Type)
      return Formatted_Edits
   is
      pragma Unreferenced (Input_Selection_Range, Options);
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use Ada.Strings.Unbounded;
   begin
      return Formatted_Edits'
        (Unit => Unit,
         Edit =>
           Text_Edit'(Location => No_Source_Location_Range,
                      Text     => Null_Unbounded_String),
         Formatted   => Libadalang.Analysis.No_Ada_Node,
         Diagnostics => Diagnostics_Vectors.Empty_Vector);
   end Format_Selection;

end Gnatformat.Formatting;
