--
--  Copyright (C) 2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatformat.Configuration;
with Langkit_Support.Generic_API.Unparsing;
with Libadalang.Analysis;

package Gitdiff is
   type Context is record
      Lal_Ctx          : Libadalang.Analysis.Analysis_Context;
      Charset          : Unbounded_String;
      Options          : Gnatformat.Configuration.Format_Options_Type;
      Unparsing_Config :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   end record;

   procedure Format_New_Lines (Base_Commit_ID : String; Ctx : Context);
end Gitdiff;
