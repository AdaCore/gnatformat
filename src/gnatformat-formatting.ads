--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

with Gnatformat.Configuration;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Slocs;

with Libadalang.Analysis;

with Prettier_Ada.Documents;

--  Package with the public API for formatting units of the supported languages

package Gnatformat.Formatting is

   function Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Format_Options : Prettier_Ada.Documents.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats the given Unit using the provided Format_Options and
   --  Configuation.

   function Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats the given Unit using the provided Format_Options and
   --  Configuration.

   ----------------------------------------------------------------------------
   --            Ada Language Server API to integrate in the IDE's           --
   ----------------------------------------------------------------------------

   type Text_Edit is
      record
         Location : Langkit_Support.Slocs.Source_Location_Range;
         Text     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Formatted_Edits is
      record
         Unit        : Libadalang.Analysis.Analysis_Unit;
         Edit        : Text_Edit;
         Formatted   : Libadalang.Analysis.Ada_Node;
         Diagnostics : Diagnostics_Vectors.Vector;
      end record;

   function Image (Edit : Formatted_Edits) return String;

   function Range_Format
     (Unit                  : Libadalang.Analysis.Analysis_Unit;
      Input_Selection_Range : Langkit_Support.Slocs.Source_Location_Range;
      Options               : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config      :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
        Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Formatted_Edits;
   --  Gnatformat library entry point for range formatting of a given Unit

end Gnatformat.Formatting;
