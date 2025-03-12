--
--  Copyright (C) 2024-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

with Gnatformat.Configuration;
with Gnatformat.Edits;

with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;
with Langkit_Support.Slocs;

with Libadalang.Analysis;

with Prettier_Ada.Documents;

--  Package with the public API for formatting units of the supported languages

package Gnatformat.Formatting is

   Internal_Error_Off_On_Invalid_Marker : exception;
   Off_On_Invalid_Marker : exception;

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

   function Range_Format
     (Unit            : Libadalang.Analysis.Analysis_Unit;
      Selection_Range : Langkit_Support.Slocs.Source_Location_Range;
      Format_Options  : Gnatformat.Configuration.Format_Options_Type;
      Configuration   :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration :=
          Gnatformat.Configuration.Default_Unparsing_Configuration)
      return Gnatformat.Edits.Formatting_Edit_Type;
   --  Range formats a section of Unit using the provided Format_Options and
   --  Configuration. The section is computed based on the bottommost node that
   --  includes Selection_Range.

end Gnatformat.Formatting;
