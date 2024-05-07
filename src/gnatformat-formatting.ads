--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;

with Gnatformat.Configuration;

with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Unparsing;

with Libadalang.Analysis;

with Prettier_Ada.Documents;

--  Package with the public API for formatting units of the supported languages

package Gnatformat.Formatting is

   function Format
     (Unit           : Langkit_Support.Generic_API.Analysis.Lk_Unit;
      Format_Options : Prettier_Ada.Documents.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats the given Unit using the provided Format_Options and
   --  Configuation.

   function Format
     (Unit           : Libadalang.Analysis.Analysis_Unit;
      Format_Options : Gnatformat.Configuration.Format_Options_Type;
      Configuration  :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Formats the given Unit using the provided Format_Options and
   --  Configuation.

end Gnatformat.Formatting;