--
--  Copyright (C) 2025-2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gnatformat.Abstract_Writers;
with Gnatformat.Configuration;
with Langkit_Support.Generic_API.Unparsing;
with Libadalang.Analysis;

package Gnatformat.Gitdiff is
   Git_Command_Failed : exception;
   --  Raised by Format_New_Lines when a git command fails or its output
   --  cannot be parsed, for instance when Base_Commit_ID is invalid.

   type Context is record
      Lal_Ctx          : Libadalang.Analysis.Analysis_Context;
      Options          : Gnatformat.Configuration.Format_Options_Type;
      --  Each file is decoded, and its edits encoded, with the charset
      --  configured in Options for it if any, otherwise with the one
      --  detected from its contents (see Gnatformat.Encodings).
      Unparsing_Config :
        Langkit_Support.Generic_API.Unparsing.Unparsing_Configuration;
   end record;

   procedure Format_New_Lines
     (Base_Commit_ID : String;
      Ctx            : Context;
      Writer         :
        in out Gnatformat.Abstract_Writers.Abstract_Writer'Class);
   --  Formats the lines added on top of Base_Commit_ID.
   --  Files that cannot be formatted are skipped and reported through
   --  Writer: as warnings when their encoding could not be determined or
   --  they start with a byte order mark, as errors (which also set the
   --  general failure flag) when they have parse errors or start with a byte
   --  order mark incompatible with their configured charset.
end Gnatformat.Gitdiff;
